module Parser where

import Data.Attoparsec.Text.Lazy (parse, maybeResult)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as L (readFile)
import Data.HashMap.Lazy (empty)
import Data.Maybe (fromMaybe)

import Types.VRScene (VRScene(..))
import Parser.VRScene (vrscene)


-- | Parse a text containing a vrscene
parseVRScene :: Text -> VRScene
parseVRScene = fromMaybe (VRScene empty empty) .
  maybeResult . parse vrscene

-- | Parse a file containing a vrscene
readVRScene :: FilePath -> IO VRScene
readVRScene = fmap parseVRScene . L.readFile
