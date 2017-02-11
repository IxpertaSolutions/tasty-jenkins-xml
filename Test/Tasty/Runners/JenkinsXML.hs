{-# LANGUAGE FlexibleInstances #-}

-- | TODO
module Test.Tasty.Runners.JenkinsXML
    ( antXMLTransformer
    )
  where

import Data.Typeable (Typeable)
import Data.Proxy (Proxy(Proxy))
import Control.Applicative (pure)
import Control.Monad (msum)

import Test.Tasty.Options
    ( IsOption(defaultValue, parseValue, optionName, optionHelp)
    , OptionSet
    , OptionDescription(Option)
    , lookupOption
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

type ReportFn = StatusMap -> IO (Time -> IO Bool)

antXmlOptions :: [OptionDescription]
antXmlReport :: OptionSet -> TestTree -> Maybe ReportFn
TestReporter antXmlOptions antXmlReport = antXMLRunner

antXMLTransformer :: [Ingredient] -> Ingredient
antXMLTransformer =
    ingredientTransformer (compatOption : antXmlOptions) $
        reportTransform antXmlTransform . applyCompatOpt
  where
    compatOption = Option (Proxy :: Proxy (Maybe CompatAntXMLPath))

    applyCompatOpt opts = case lookupOption opts of
        Nothing -> opts
        Just (CompatAntXMLPath path) ->
            setOption (Just (AntXMLPath path)) opts

    antXmlTransform opts testTree smap totalTime retVal =
        case antXmlReport opts testTree of
            Nothing -> return retVal
            Just reportFn -> do
                k <- reportFn smap
                k totalTime

reportTransform
    :: (OptionSet -> TestTree -> StatusMap -> Time -> Bool -> IO Bool)
    -> OptionSet -> TestTree -> ReportFn -> ReportFn
reportTransform f opts testTree reportFn smap =
    reportFn smap >>= \k -> return $ \totalTime ->
        k totalTime >>= f opts testTree smap totalTime

ingredientTransformer
    :: [OptionDescription]
    -> (OptionSet -> TestTree -> ReportFn -> ReportFn)
    -> [Ingredient] -> Ingredient
ingredientTransformer options transform ingredients =
    TestManager (options ++ existingOptions) $
        tryIngredients' transform ingredients
  where
    existingOptions = flip concatMap ingredients $ \ingredient ->
        case ingredient of
            TestReporter opts _ -> opts
            TestManager opts _ -> opts

tryIngredient'
    :: (OptionSet -> TestTree -> ReportFn -> ReportFn)
    -> Ingredient -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredient' f (TestReporter _ report) opts testTree = do -- Maybe monad
    reportFn <- report opts testTree
    return $ launchTestTree opts testTree $ f opts testTree reportFn
tryIngredient' _ (TestManager _ manage) opts testTree =
    manage opts testTree

tryIngredients'
    :: (OptionSet -> TestTree -> ReportFn -> ReportFn)
    -> [Ingredient] -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredients' f ins opts tree =
    msum $ map (\i -> tryIngredient' f i opts tree) ins
