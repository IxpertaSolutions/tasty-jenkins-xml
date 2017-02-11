{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import System.Directory (doesFileExist)
import System.Environment (withArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))

import Data.ByteString.Lazy.Char8 (unpack)
import System.IO.Capture (capture)
import System.Posix.Process (ProcessStatus(Exited))
import Test.Hspec
    ( Selector
    , around_
    , describe
    , hspec
    , it
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldThrow
    )
import Test.Mockery.Directory (inTempDirectory)
import Test.Tasty
    ( TestTree
    , defaultMainWithIngredients
    , defaultIngredients
    , testGroup
    )
import Test.Tasty.HUnit (assert, testCase)
import Test.Tasty.Runners.JenkinsXML (antXMLTransformer)


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = hspec $ do
    describe "antXMLTransformer" $ around_ inTempDirectory $ do
        it "lists options in --help" $ do
            (out, err, exc, status) <- capture $
                withArgs ["--help"] $
                    tastyMain `shouldThrow` exitSuccess
            status `shouldBe` Just (Exited ExitSuccess)
            (err, exc) `shouldBe` ("", "")
            unpack out `shouldContain` "Usage:"
            unpack out `shouldContain` "--xml"
            unpack out `shouldContain` "--jxml"
            unpack out `shouldContain` "--exit-success"
        it "reports failure when some test fails" $ do
            (out, err, exc, status) <- capture $
                withArgs [] $
                    tastyMain `shouldThrow` exitFailure (Just 1)
            status `shouldBe` Just (Exited ExitSuccess)
            (err, exc) `shouldBe` ("", "")
            unpack out `shouldContain` "\n1 out of 2 tests failed"
            doesFileExist "tasty.xml" `shouldReturn` False
        it "exits successfully even when some test fails (--exit-success)" $ do
            (out, err, exc, status) <- capture $
                withArgs ["--exit-success"] $
                    tastyMain `shouldThrow` exitSuccess
            status `shouldBe` Just (Exited ExitSuccess)
            (err, exc) `shouldBe` ("", "")
            unpack out `shouldContain` "\n1 out of 2 tests failed"
            doesFileExist "tasty.xml" `shouldReturn` False
        forM_ ["--xml", "--jxml"] $ \flag ->
            it ("writes xml in addition to console output (" ++ flag ++ ")") $ do
                (out, err, exc, status) <- capture $
                    withArgs [flag, "tasty.xml"] $
                        tastyMain `shouldThrow` exitFailure (Just 1)
                status `shouldBe` Just (Exited ExitSuccess)
                (err, exc) `shouldBe` ("", "")
                unpack out `shouldContain` "\n1 out of 2 tests failed"
                xml <- readFile "tasty.xml"
                xml `shouldContain` "<?xml"
                xml `shouldContain` "errors=\"0\""
                xml `shouldContain` "failures=\"1\""
                xml `shouldContain` "tests=\"2\""

tastyMain :: IO ()
tastyMain = defaultMainWithIngredients ingredients tastyTests
  where
    ingredients = [antXMLTransformer defaultIngredients]

tastyTests :: TestTree
tastyTests = testGroup "group1"
    [ testCase "test1" $ assert ()
    , testCase "test2" $ assert ("fail" :: String)
    ]

exitSuccess :: Selector ExitCode
exitSuccess = \case
    ExitSuccess -> True
    ExitFailure _ -> False

exitFailure :: Maybe Int -> Selector ExitCode
exitFailure code = \case
    ExitSuccess -> False
    ExitFailure code' -> maybe True (code' ==) code
