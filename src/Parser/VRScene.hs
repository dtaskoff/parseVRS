module Parser.VRScene where

import Data.Attoparsec.Text
import Control.Applicative((<|>))
import Data.HashMap.Lazy (empty, insert)

import Parser.Util (skipLine)
import Parser.Node (node)
import Parser.GeomStaticMesh (geomStaticMesh)

import Types.Node (nodeName)
import Types.GeomStaticMesh (gsmName)
import Types.VRScene (VRScene(..))


-- | Parse a VRScene
-- Currently extracting information about nodes and meshes only
vrscene :: Parser VRScene
vrscene = helper empty empty
  where helper nodes meshes = vrsNode nodes meshes <|>
          vrsGSM nodes meshes <|>
          (endOfInput *> (pure $ VRScene nodes meshes)) <|>
          (skipLine *> helper nodes meshes)
        vrsNode ns ms = do
          n       <- node
          helper (insert (nodeName n) n ns) ms
        vrsGSM  ns ms = do
          m       <- geomStaticMesh
          helper ns (insert (gsmName m) m ms)
