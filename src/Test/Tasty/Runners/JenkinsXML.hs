{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Tasty.Runners.JenkinsXML
    ( CompatAntXMLPath(..)
    , ExitSuccess(..)
    , addCompatOpt
    , addExitOpt
    , jenkinsXMLRunner
    )
  where

import Control.Applicative (pure)
import Data.Bool (Bool(True, False), (||))
import Data.Function ((.), ($), flip)
import Data.Functor (Functor(fmap))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import System.IO (FilePath)

import Test.Tasty.Ingredients (Ingredient(TestReporter), composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)
import Test.Tasty.Options
    ( IsOption(defaultValue, parseValue, optionName, optionHelp, optionCLParser)
    , OptionDescription(Option)
    , lookupOption
    , mkFlagCLParser
    , safeRead
    , setOption
    )
import Test.Tasty.Runners.AntXML (antXMLRunner, AntXMLPath(AntXMLPath))


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
    optionCLParser = mkFlagCLParser mempty (ExitSuccess True)

addCompatOpt :: Ingredient -> Ingredient
addCompatOpt reporter =
    TestReporter (compatOpt : optDesc) (runner . applyCompatOpt)
  where
    TestReporter optDesc runner = reporter
    compatOpt = Option (Proxy :: Proxy (Maybe CompatAntXMLPath))
    applyCompatOpt opts = case lookupOption opts of
        Nothing -> opts
        Just (CompatAntXMLPath path) ->
            setOption (Just (AntXMLPath path)) opts

addExitOpt :: Ingredient -> Ingredient
addExitOpt reporter =
    TestReporter (exitOpt : optDesc) (mapExit exit runner)
  where
    TestReporter optDesc runner = reporter
    exitOpt = Option (Proxy :: Proxy ExitSuccess)
    exit opts retVal = retVal || isExitSuccess (lookupOption opts)
    mapExit f run o tt = run o tt <&> \rf s -> rf s <&> \r t -> r t <&> f o

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | An extended version of 'antXMLRunner' that also outputs to console and
-- implements two additions to be more practically useful:
--
--  * @--jxml@ alias for @--xml@ for @test-framework@ compatibility,
--
--  * @--exit-success@ to distinguish between /failed/ and /unstable/ builds
--    in Jenkins CI.
jenkinsXMLRunner :: Ingredient
jenkinsXMLRunner = addExitOpt . addCompatOpt
    $ antXMLRunner `composeReporters` consoleTestReporter
