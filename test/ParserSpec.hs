{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Attoparsec.Text
import Data.Array.IArray (listArray)
import Data.Text (Text)

import Parser.Util
import Parser.GeomStaticMesh
import Types.GeomStaticMesh


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

    describe "listInt" $
      it "parses a ListInt field" $
        ("ListInt(0, 1, 1, 2, 3, 5, 8, 13)" :: Text) ~> listInt
          `shouldParse` [0, 1, 1, 2, 3, 5, 8, 13]

    describe "end" $
      it "parse a semicolon terminated text" $ do
        ("ListInt(0, 1, 1, 2, 3, 5, 8, 13);" :: Text) ~> end listInt
          `shouldParse` [0, 1, 1, 2, 3, 5, 8, 13]
        ("ListInt(0, 1, 1, 2, 3, 5, 8, 13);rest" :: Text) ~?> end listInt
          `leavesUnconsumed` "rest"

    describe "field" $
      it "parse a semicolon terminated field which may be missing" $ do
        ("ListInt(0, 1, 1, 2, 3, 5, 8, 13);" :: Text) ~> field [] listInt
          `shouldParse` [0, 1, 1, 2, 3, 5, 8, 13]
        ("Vector(0, 1, 1);" :: Text) ~> field [] listInt
          `shouldParse` []
        ("Vector(0, 1, 1);" :: Text) ~?> field [] listInt
          `leavesUnconsumed` "Vector(0, 1, 1);"

    describe "skipLine" $
      it "discards a line" $
        ("line#1\nline#2" :: Text) ~?> skipLine
          `leavesUnconsumed` "line#2"

  describe "GeomStaticMesh" $ do
    describe "vertices" $
      it "parses a vertices field" $
        ("vertices=ListVector(\
  \\n\tVector(-4.156086, -7.002372, -0.04999952),\
  \\n\tVector(-4.156086, -7.002372, -1.828932)\
  \\n)" :: Text) ~> vertices
          `shouldParse` listArray (0, 1)
              [ (-4.156086, -7.002372, -0.04999952)
              , (-4.156086, -7.002372, -1.828932)
              ]

    describe "faces" $
      it "parses a faces field" $
        ("faces=ListInt(\
  \\n\t2,1,0,0,292,2,222,294,243,222\
  \\n)" :: Text) ~> faces
          `shouldParse` [2, 1, 0, 0, 292, 2, 222, 294, 243, 222]

    describe "normals" $
      it "parses a normals field" $
        ("normals=ListVector(\
  \\n\tVector(0, -1, 0),\
  \\n\tVector(-0.00228692, -0.6251184, 0.7805266)\
  \\n\t)" :: Text) ~> normals
          `shouldParse` listArray (0, 1)
              [ (0, -1, 0)
              , (-0.00228692, -0.6251184, 0.7805266)
              ]

    describe "faceNormals" $
      it "parses a faceNormals field" $
        ("faceNormals=ListInt(\
  \\n\t3,2,0,0,659,3,481,667,529,481\
  \\n)" :: Text) ~> faceNormals
          `shouldParse` [3, 2, 0, 0, 659, 3, 481, 667, 529, 481]

    describe "mapChannels" $
      it "parses a map_channels field" $
        ("map_channels=List(\
  \\n\tList(\
  \\n\t\t1,\
  \\n\t\tListVector(\
  \\n\t\t\tVector(0.3225643, 0.747669, 0.0629372),\
  \\n\t\t\tVector(0.3225643, 0.6736208, 0.0629372)\
  \\n\t\t),\
  \\n\t\tListInt(\
  \\n\t\t\t2,1,0,0,292,2,222,294,243,222\
  \\n\t\t)\
  \\n\t)\
  \\n)" :: Text) ~> mapChannels
          `shouldParse` [ ( 1
                          , listArray (0, 1) [ (0.3225643, 0.747669, 0.0629372)
                                             , (0.3225643, 0.6736208, 0.0629372)
                                             ]
                          , [2,1,0,0,292,2,222,294,243,222]
                          )
                        ]

    describe "edgeVisibility" $
      it "parses an edge_visibility field" $
        ("edge_visibility=ListInt(\
  \\n\t460186843,460175067,460175067\
  \\n)" :: Text) ~> edgeVisibility
          `shouldParse` [460186843, 460175067, 460175067]

    describe "faceMtlIDs" $
      it "parses a face_mtlIDs field" $
        ("face_mtlIDs=ListInt(\
  \\n\t0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0\
  \\n)" :: Text) ~> faceMtlIDs
          `shouldParse` [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    describe "smoothDerivs" $
      it "parses a smooth_derivs field" $
        ("smooth_derivs=List()" :: Text) ~> smoothDerivs
          `shouldParse` []

    describe "dynamicGeometry" $
      it "parses a dynamic_geometry field" $
        ("dynamic_geometry=0" :: Text) ~> dynamicGeometry
          `shouldParse` False

    describe "geomStaticMesh" $
      it "parses a GeomStaticMesh plugin" $
        ("GeomStaticMesh Mesh_1416943569 {\
  \\n\tvertices=ListVector(\
  \\n\t\tVector(-4.156086, -7.002372, -0.04999952),\
  \\n\t\tVector(-4.156086, -7.002372, -1.828932)\
  \\n\t);\
  \\n\tfaces=ListInt(\
  \\n\t\t2,1,0,0,292,2,222,294,243,222\
  \\n\t);\
  \\n\tnormals=ListVector(\
  \\n\t\tVector(0, -1, 0),\
  \\n\t\tVector(-0.00228692, -0.6251184, 0.7805266)\
  \\n\t);\
  \\n\tfaceNormals=ListInt(\
  \\n\t\t3,2,0,0,659,3,481,667,529,481\
  \\n\t);\
  \\n\tmap_channels=List(\
  \\n\t\tList(\
  \\n\t\t\t1,\
  \\n\t\t\tListVector(\
  \\n\t\t\t\tVector(0.3225643, 0.747669, 0.0629372),\
  \\n\t\t\t\tVector(0.3225643, 0.6736208, 0.0629372)\
  \\n\t\t\t),\
  \\n\t\t\tListInt(\
  \\n\t\t\t\t2,1,0,0,292,2,222,294,243,222\
  \\n\t\t\t)\
  \\n\t\t)\
  \\n\t);\
  \\n\tedge_visibility=ListInt(\
  \\n\t\t460186843,460175067,460175067\
  \\n\t);\
  \\n\tface_mtlIDs=ListInt(\
  \\n\t\t0,0,0,0,0,0,0,0,0,0\
  \\n\t);\
  \\n\tsmooth_derivs=List();\
  \\n\tdynamic_geometry=0;\
  \\n}" :: Text) ~> geomStaticMesh
          `shouldParse` GeomStaticMesh
            "Mesh_1416943569"
            (listArray (0, 1) [ (-4.156086, -7.002372, -0.04999952)
                              , (-4.156086, -7.002372, -1.828932)
                              ])
            [2, 1, 0, 0, 292, 2, 222, 294, 243, 222]
            (listArray (0, 1) [ (0, -1, 0)
                              , (-0.00228692, -0.6251184, 0.7805266)
                              ])
            [3, 2, 0, 0, 659, 3, 481, 667, 529, 481]
            [( 1
             , listArray (0, 1) [ (0.3225643, 0.747669, 0.0629372)
                                , (0.3225643, 0.6736208, 0.0629372)
                                ]
             , [2,1,0,0,292,2,222,294,243,222]
             )]
            [460186843, 460175067, 460175067]
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            []
            False
