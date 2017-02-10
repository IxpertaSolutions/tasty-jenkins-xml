{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

main :: IO ()
main = hspec $ do
    describe "antXMLTransformer" $
        around_ inTempDirectory $ do
            it "doesn't break --help" $ do
                (out, err, exc, status) <- capture $
                    withArgs ["--help"] $
                        tastyMain `shouldThrow` exitSuccess
                status `shouldBe` Just (Exited ExitSuccess)
                unpack out `shouldContain` "Usage:"
                (err, exc) `shouldBe` ("", "")
            it "returns failure when some test fails" $ do
                (out, err, exc, status) <- capture $
                    withArgs [] $
                        tastyMain `shouldThrow` exitFailure (Just 1)
                status `shouldBe` Just (Exited ExitSuccess)
                unpack out `shouldContain` "\n1 out of 2 tests failed"
                (err, exc) `shouldBe` ("", "")
                doesFileExist "tasty.xml" `shouldReturn` False
            it "writes xml in addition to console output" $ do
                (out, err, exc, status) <- capture $
                    withArgs ["--xml", "tasty.xml"] $
                        tastyMain `shouldThrow` exitFailure (Just 1)
                status `shouldBe` Just (Exited ExitSuccess)
                unpack out `shouldContain` "\n1 out of 2 tests failed"
                (err, exc) `shouldBe` ("", "")
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
