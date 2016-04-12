module Parser.Util where

import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Array.IArray (Array, listArray)


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
