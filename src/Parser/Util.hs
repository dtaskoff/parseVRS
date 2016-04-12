{-# LANGUAGE OverloadedStrings #-}
module Parser.Util where

import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Array.IArray (Array, listArray)

import Types.Internal


-- | Parse between two characters
-- and discard them from the result
between :: Char -> Char -> Parser a -> Parser a
between o c p = char o *> skipSpace *> p <* skipSpace <* char c

-- | Parse between `(` and `)`
-- and discard them from the result
parens :: Parser a -> Parser a
parens p = between '(' ')' p

-- | Parse between `{` and `}`
-- and discard them from the result
braces :: Parser a -> Parser a
braces p = between '{' '}' p

-- | Skip comma with following spaces
comma :: Parser ()
comma = char ',' *> skipSpace

-- | Parse comma-space separated, i.e. "one,\ntwo,\nthree"
sepByComma :: Parser a -> Parser [a]
sepByComma = (`sepBy` comma)

-- | Parse a text of the form "name(a, b, c)"
-- and return a list containing the results
listOf :: Text -> Parser a -> Parser [a]
listOf name p = string name *> (parens $ sepByComma p)

-- | Parse a text of the form "name(a, b, c)" -> [a, b, c]
-- and return an IArray containing the results
arrayOf :: Text -> Parser a -> Parser (Array Int a)
arrayOf name p = (subtract 1 . length >>= listArray . ((,) 0)) <$> listOf name p

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

-- | Parse a ListVector field
listVector :: Parser (Array Int (Double, Double, Double))
listVector = arrayOf "ListVector" triple

-- | Parse a ListVector containing Vector fields
arrayVector :: Parser (Array Int Vector)
arrayVector = listVector

-- | Parse a ListVector containing Vector fields for normals
arrayNormal :: Parser (Array Int Normal)
arrayNormal = listVector

-- | Parse a ListVector containing Vector fields for colours
arrayColour :: Parser (Array Int Colour)
arrayColour = listVector
