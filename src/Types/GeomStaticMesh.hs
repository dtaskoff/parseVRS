module Types.GeomStaticMesh where

import Data.Array.IArray (Array)
import Data.Text (Text)

import Types.Internal


type Vertices        = Array Int Vector
type Faces           = [Int]
type Normals         = Array Int Normal
type FaceNormals     = [Int]
type MapChannels     = [(Int, Array Int Colour, [Int])]
type EdgeVisibility  = [Int]
type FaceMtlIDs      = [Int]
type SmoothDerivs    = [Int]
type DynamicGeometry = Bool

data GeomStaticMesh = GeomStaticMesh
  { gsmName            :: Text
  , gsmVertices        :: Vertices
  , gsmFaces           :: Faces
  , gsmNormals         :: Normals
  , gsmFaceNormals     :: FaceNormals
  , gsmMapChannels     :: MapChannels
  , gsmEdgeVisibility  :: EdgeVisibility
  , gsmFaceMtlIDs      :: FaceMtlIDs
  , gsmSmoothDerivs    :: SmoothDerivs 
  , gsmDynamicGeometry :: DynamicGeometry
  } deriving (Eq, Show)
