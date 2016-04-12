{-# LANGUAGE OverloadedStrings #-}
module Parser.Internal where

import Data.Attoparsec.Text

import Parser.Util
import Types.Internal


-- | Parse a Vector triple
triple :: Parser (Double, Double, Double)
triple = (\[x, y, z] -> (x, y, z)) <$>
  listOf "Vector" double

-- | Parse a Vector field
vector :: Parser Vector
vector = triple

-- | Parse a Vector field for normal
normal :: Parser Normal
normal = triple

-- | Parse a Vector field for colour
colour :: Parser Colour
colour = triple
