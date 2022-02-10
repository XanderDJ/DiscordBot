module Commands.ParsersSpec (spec) where

import Test.Hspec

import Commands.Parsers
import Text.Parsec

spec :: Spec
spec = do
    bidParsingSpec

bidParsingSpec :: SpecWith ()
bidParsingSpec = describe "Parsing bids" $ do
        it "parses \"lb 2000\" to Just 2000" $ do
            parse bidP "parsing bid" "lb 2000" `shouldBe` Right (Just 2000)
        it "parses \"lb 2.0\" to Just 2000" $ do
            parse bidP "parsing bid" "lb 2.0" `shouldBe` Right (Just 2000)
        it "parses \"lb 2.0k\" to Just 2000" $ do
            parse bidP "parsing bid" "lb 2.0k" `shouldBe` Right (Just 2000)
        it "parses \"lb 2.01\" to Just 2010" $ do
            parse bidP "parsing bid" "lb 2.01" `shouldBe` Right (Just 2010)
        it "parses \"lb\" to Nothing" $ do
            parse bidP "parsing bid" "lb" `shouldBe` Right Nothing
        it "parses \"lb .5\" to Nothing" $ do
            parse bidP "parsing bid" "lb .5" `shouldBe` Right Nothing
        it "parses \"lb . \" to Nothing" $ do
            parse bidP "parsing bid" "lb . " `shouldBe` Right Nothing