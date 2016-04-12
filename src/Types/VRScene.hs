module Types.VRScene where

import Data.Array.IArray (Array)
import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)

import Types.Node (Node)
import Types.GeomStaticMesh (GeomStaticMesh)


type Nodes  = HashMap Text Node
type Meshes = HashMap Text GeomStaticMesh

data VRScene = VRScene
  { vrsNodes            :: Nodes
  , vrsGeomStaticMeshes :: Meshes
  } deriving (Eq, Show)
