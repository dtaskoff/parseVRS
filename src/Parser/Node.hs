{-# LANGUAGE OverloadedStrings #-}
module Parser.Node where

import Data.Attoparsec.Text
import Control.Applicative (liftA2)
import Data.Char (isSpace)

import Types.Node
import Parser.Util


-- The following parser parse their corresponding
-- fields in the Node plugin

transform :: Parser Transform
transform = string "transform=Transform" *>
  parens (liftA2 (,) matrix (comma *> vector))

geometry :: Parser Geometry
geometry = string "geometry=" *> takeTill (== ';')

material :: Parser Material
material = string "material=" *> takeTill (== ';')

nsamples :: Parser NSamples
nsamples = string "nsamples=" *> decimal

visible :: Parser Visible
visible = string "visible=" *>
  fmap (== (1::Int)) decimal

primaryVisibility :: Parser PrimaryVisibility
primaryVisibility = string "primary_visibility=" *>
  fmap (== (1::Int)) decimal

node :: Parser Node
node = do
  _ <- string "Node "
  name <- takeTill isSpace
  skipSpace
  braces $
    Node name <$> 
      field undefined transform <*>
      field "" geometry <*>
      field "" material <*>
      field 0 nsamples <*>
      field False visible <*>
      field False primaryVisibility
