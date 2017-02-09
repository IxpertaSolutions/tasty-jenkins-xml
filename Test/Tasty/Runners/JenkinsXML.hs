-- | TODO
module Test.Tasty.Runners.JenkinsXML
    ( antXMLTransformer
    )
  where

import Control.Monad (msum)

import Test.Tasty.Options
    ( OptionSet
    , OptionDescription
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
    )


type ReportFn = IO (Time -> IO Bool)

antXmlOptions :: [OptionDescription]
antXmlReport :: OptionSet -> TestTree -> Maybe (StatusMap -> ReportFn)
TestReporter antXmlOptions antXmlReport = antXMLRunner

antXMLTransformer :: [Ingredient] -> Ingredient
antXMLTransformer =
    ingredientTransformer antXmlOptions $ reportTransform antXmlTransform
  where
    antXmlTransform opts testTree smap totalTime retVal =
        case antXmlReport opts testTree of
            Nothing -> return retVal
            Just reportFn -> do
                k <- reportFn smap
                retVal' <- k totalTime
                return retVal'

reportTransform
    :: (OptionSet -> TestTree -> StatusMap -> Time -> Bool -> IO Bool)
    -> OptionSet -> TestTree -> StatusMap -> ReportFn -> ReportFn
reportTransform f opts testTree smap reportFn =
  reportFn >>= \k -> return $ \totalTime ->
    k totalTime >>= f opts testTree smap totalTime

ingredientTransformer
    :: [OptionDescription]
    -> (OptionSet -> TestTree -> StatusMap -> ReportFn -> ReportFn)
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
        :: (OptionSet -> TestTree -> StatusMap -> ReportFn -> ReportFn)
        -> Ingredient -> OptionSet -> TestTree -> Maybe (IO Bool)
    tryIngredient' f (TestReporter _ report) opts testTree = do -- Maybe monad
        reportFn <- report opts testTree
        return $ launchTestTree opts testTree $ \smap ->
            f opts testTree smap $ reportFn smap
    tryIngredient' _ (TestManager _ manage) opts testTree =
        manage opts testTree

    tryIngredients'
        :: (OptionSet -> TestTree -> StatusMap -> ReportFn -> ReportFn)
        -> [Ingredient] -> OptionSet -> TestTree -> Maybe (IO Bool)
    tryIngredients' f ins opts tree =
        msum $ map (\i -> tryIngredient' f i opts tree) ins
