{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Attoparsec.Text
import Data.Array.IArray (listArray)
import Data.Text (Text)

import Parser.Util
import Parser.GeomStaticMesh


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
  
    describe "triple" $ do
      it "parses a Vector triple" $
        ("Vector(1, 2, 3)" :: Text) ~> triple
          `shouldParse` (1, 2, 3)
      it "parses a Vector triple (scientific notation test)" $
        ("Vector(-4.768372e-007, -0.002371788, -1.665215)" :: Text) ~> triple
          `shouldParse` (-4.768372e-007, -0.002371788, -1.665215)

    describe "vector" $
      it "parses a Vector field" $
        ("Vector(1, 2, 3)" :: Text) ~> vector
          `shouldParse` (1, 2, 3)

    describe "normal" $
      it "parses a Vector field for normal" $
        ("Vector(1, 2, 3)" :: Text) ~> normal
          `shouldParse` (1, 2, 3)

    describe "colour" $
      it "parses a Vector field for colour" $
        ("Vector(1, 2, 3)" :: Text) ~> colour
          `shouldParse` (1, 2, 3)

    describe "listVector" $
      it "parses a ListVector field" $
        ("ListVector(\
  \\n\tVector(1, 2, 3),\
  \\n\tVector(3, 4, 5),\
  \\n\tVector(5, 6, 7))" :: Text) ~> listVector
          `shouldParse` listArray (0, 2)
              [(1, 2, 3), (3, 4, 5), (5, 6, 7)]

    describe "arrayVector" $
      it "parses a ListVector field with vertices" $
        ("ListVector(\
  \\n\tVector(1, 2, 3),\
  \\n\tVector(3, 4, 5),\
  \\n\tVector(5, 6, 7))" :: Text) ~> arrayVector
          `shouldParse` listArray (0, 2)
              [(1, 2, 3), (3, 4, 5), (5, 6, 7)]

    describe "arrayNormal" $
      it "parses a ListVector field with normals" $
        ("ListVector(\
  \\n\tVector(1, 2, 3),\
  \\n\tVector(3, 4, 5),\
  \\n\tVector(5, 6, 7))" :: Text) ~> arrayNormal
          `shouldParse` listArray (0, 2)
              [(1, 2, 3), (3, 4, 5), (5, 6, 7)]

    describe "arrayColour" $
      it "parses a ListVector field with colours" $
        ("ListVector(\
  \\n\tVector(1, 2, 3),\
  \\n\tVector(3, 4, 5),\
  \\n\tVector(5, 6, 7))" :: Text) ~> arrayColour
          `shouldParse` listArray (0, 2)
              [(1, 2, 3), (3, 4, 5), (5, 6, 7)]
