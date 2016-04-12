{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Attoparsec.Text
import Data.Array.IArray (listArray)
import Data.Text (Text)

import Parser.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = let word = many1 letter in do
  describe "Util" $ do
    describe "between" $
      it "parses text between two characters" $ do
        ("4bet2ween" :: Text) ~> between '4' '2' word
          `shouldParse` "bet"
        ("4bet2ween" :: Text) ~?> between '4' '2' word
          `leavesUnconsumed` "ween"

    describe "parens" $
      it "parses letters between parens" $ do
        ("(parens)" :: Text) ~> parens word
          `shouldParse` "parens"
        ("(\n\tparens\n)" :: Text) ~> parens word
          `shouldParse` "parens"

    describe "braces" $
      it "parses text between braces" $ do
        ("{braces}" :: Text) ~> braces word
          `shouldParse` "braces"
        ("{\n\tbraces\n}" :: Text) ~> braces word
          `shouldParse` "braces"

    describe "comma" $
      it "skips comma with following spaces" $ do
        (", \n \t rest" :: Text) ~> comma
          `shouldParse` ()
        (", \n \t rest" :: Text) ~?> comma
          `leavesUnconsumed` "rest"

    describe "sepByComma" $
      it "parses comma-separated decimals" $
        ("0, 1,   1, 2, 3,5,8,\n\t13" :: Text) ~> sepByComma decimal
          `shouldParse` [0,1,1,2,3,5,8,13]

    describe "listOf" $
      it "parses a list of decimals" $
        ("List(\n\t0, 1, 1, 2, 3, 5, 8, 13\n)" :: Text) ~> listOf "List" decimal
          `shouldParse` [0,1,1,2,3,5,8,13]
        
    describe "arrayOf" $
      it "parses a list of decimals to an array" $
        ("List(\n\t0, 1, 1, 2, 3, 5, 8, 13\n)" :: Text) ~> arrayOf "List" decimal
          `shouldParse` listArray (0,7) [0,1,1,2,3,5,8,13]
