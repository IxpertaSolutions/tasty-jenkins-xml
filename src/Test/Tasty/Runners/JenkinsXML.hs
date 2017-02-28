{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Tasty.Runners.JenkinsXML
    ( CompatAntXMLPath(..)
    , jenkinsXMLTransformer
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), msum, join)
import Data.Bool (Bool(True, False), (||))
import Data.Foldable (concatMap)
import Data.Function ((.), ($), flip)
import Data.Functor (fmap)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Traversable (mapM)
import Data.Typeable (Typeable)
import System.IO (IO, FilePath)

import Test.Tasty.Options
    ( IsOption(defaultValue, parseValue, optionName, optionHelp, optionCLParser)
    , OptionSet
    , OptionDescription(Option)
    , flagCLParser
    , lookupOption
    , safeRead
    , setOption
    )
import Test.Tasty.Runners
    ( Ingredient(TestReporter, TestManager)
    , StatusMap
    , TestTree
    , Time
    , launchTestTree
    )
import Test.Tasty.Runners.AntXML
    ( antXMLRunner
    , AntXMLPath(AntXMLPath)
    )


newtype CompatAntXMLPath = CompatAntXMLPath FilePath deriving Typeable

instance IsOption (Maybe CompatAntXMLPath) where
    defaultValue = Nothing
    parseValue = Just . Just . CompatAntXMLPath
    optionName = pure "jxml"
    optionHelp = pure "An alias for --xml"

newtype ExitSuccess = ExitSuccess { isExitSuccess :: Bool } deriving Typeable

instance IsOption ExitSuccess where
    defaultValue = ExitSuccess False
    parseValue = fmap ExitSuccess . safeRead
    optionName = pure "exit-success"
    optionHelp = pure "Exit with status 0 even if some tests failed"
    optionCLParser = flagCLParser Nothing (ExitSuccess True)

type ReportFn = StatusMap -> IO (Time -> IO Bool)

antXmlOptions :: [OptionDescription]
antXmlReport :: OptionSet -> TestTree -> Maybe ReportFn
TestReporter antXmlOptions antXmlReport = antXMLRunner

-- | This 'Ingredient' transformer adds the possibility to produce a JUnit XML
-- file __in addition to__ the output producer by another 'Ingredient'.
-- Internally it invokes 'Test.Tasty.Runners.AntXML.antXMLRunner'.
--
-- To be practically useful, it implements two additions:
--
--  * @--jxml@ alias for @--xml@ for @test-framework@ compatibility,
--
--  * @--exit-success@ to distinguish between /failed/ and /unstable/ builds
--    in Jenkins CI.
jenkinsXMLTransformer :: [Ingredient] -> Ingredient
jenkinsXMLTransformer =
    testReporterTransformer (exitOption : compatOption : antXmlOptions) $
        reportTransform antXmlTransform . applyCompatOpt
  where
    exitOption = Option (Proxy :: Proxy ExitSuccess)
    compatOption = Option (Proxy :: Proxy (Maybe CompatAntXMLPath))

    applyCompatOpt opts = case lookupOption opts of
        Nothing -> opts
        Just (CompatAntXMLPath path) ->
            setOption (Just (AntXMLPath path)) opts

    antXmlTransform opts testTree smap totalTime = fmap exit . antXml
      where
        exit retVal = retVal || isExitSuccess (lookupOption opts)

        antXml retVal = fromMaybe retVal `fmap` tryReport antXmlReport

        tryReport r =
            (join . fmap ($ totalTime) . ($ smap)) `mapM` r opts testTree

-- Combinator for transforming the final report/record callback of a
-- 'TestReporter'.
reportTransform
    :: (OptionSet -> TestTree -> StatusMap -> Time -> Bool -> IO Bool)
    -> OptionSet -> TestTree -> ReportFn -> ReportFn
reportTransform f opts testTree reportFn smap =
    reportFn smap >>= \k -> pure $ \totalTime ->
        k totalTime >>= f opts testTree smap totalTime

-- Combinator for building ingredient transformers that change the behaviour
-- of an existing 'TestReporter' ingredient.
testReporterTransformer
    :: [OptionDescription]
    -- ^ Additional command-line options
    -> (OptionSet -> TestTree -> ReportFn -> ReportFn)
    -- ^ Function to transform the 'ReportFn' of a 'TestReporter', see
    -- 'tryIngredient'' and 'launchTestTree' for details.
    -> [Ingredient]
    -- ^ Ingredients to transform and run.
    -> Ingredient
testReporterTransformer options transform ingredients =
    TestManager (options <> existingOptions) $
        tryIngredients' transform ingredients
  where
    existingOptions = flip concatMap ingredients $ \ingredient ->
        case ingredient of
            TestReporter opts _ -> opts
            TestManager opts _ -> opts

-- | Modified version of 'Test.Tasty.Ingredients.tryIngredient' that
-- transforms the 'ReportFn' by a given function, if the 'Ingredient' happens
-- to be a 'TestReporter'. 'TestManager' is left unmodified.
tryIngredient'
    :: (OptionSet -> TestTree -> ReportFn -> ReportFn)
    -> Ingredient -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredient' f (TestReporter _ report) opts testTree = do -- Maybe monad
    reportFn <- report opts testTree
    pure $ launchTestTree opts testTree $ f opts testTree reportFn
tryIngredient' _ (TestManager _ manage) opts testTree =
    manage opts testTree

-- | Modified version of 'Test.Tasty.Ingredients.tryIngredients' that
-- transforms the 'ReportFn' by a given function, if the 'Ingredient' happens
-- to be a 'TestReporter'. 'TestManager' is left unmodified.
tryIngredients'
    :: (OptionSet -> TestTree -> ReportFn -> ReportFn)
    -> [Ingredient] -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredients' f ins opts tree =
    msum $ fmap (\i -> tryIngredient' f i opts tree) ins
