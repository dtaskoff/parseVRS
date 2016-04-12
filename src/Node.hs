module Node where

import Types
import Data.Text (Text)


type Transform         = (Matrix, Vector)
type Geometry          = Text
type Material          = Text
type NSamples          = Int
type Visible           = Bool
type PrimaryVisibility = Bool


data Node = Node
  { nodeName              :: Text
  , nodeTransform         :: Transform
  , nodeGeometry          :: Geometry
  , nodeMaterial          :: Material
  , nodeNSamples          :: NSamples
  , nodeVisible           :: Visible
  , nodePrimaryVisibility :: PrimaryVisibility
  } deriving (Eq, Show)
