{-# LANGUAGE OverloadedStrings #-}
module Parser.GeomStaticMesh where

import Data.Attoparsec.Text
import Control.Applicative (liftA3)
import Data.Char (isSpace)

import Types.GeomStaticMesh
import Parser.Util

{-
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
-}

-- The following parser parse their corresponding
-- fields in the GeomStaticMesh plugin

vertices :: Parser Vertices
vertices = string "vertices=" *> arrayVector

faces :: Parser Faces
faces = string "faces=" *> listInt

normals :: Parser Normals
normals = string "normals=" *> arrayNormal

faceNormals :: Parser FaceNormals
faceNormals = string "faceNormals=" *> listInt

mapChannels :: Parser MapChannels
mapChannels = string "map_channels=" *>
  (listOf "List" $ do
    _ <- string "List"
    parens $
      liftA3 (,,) decimal
                  (comma *> listVector)
                  (comma *> listInt))

edgeVisibility :: Parser EdgeVisibility
edgeVisibility = string "edge_visibility=" *> listInt

faceMtlIDs :: Parser FaceMtlIDs
faceMtlIDs = string "face_mtlIDs=" *> listInt

smoothDerivs :: Parser SmoothDerivs
smoothDerivs = string "smooth_derivs=" *> listOf "List" decimal

dynamicGeometry :: Parser DynamicGeometry
dynamicGeometry = string "dynamic_geometry=" *> fmap (== (1::Int)) decimal

geomStaticMesh :: Parser GeomStaticMesh
geomStaticMesh = do
  _ <- string "GeomStaticMesh "
  name <- takeTill isSpace
  skipSpace
  braces $
    GeomStaticMesh name <$> 
      field undefined vertices <*> 
      field [] faces <*> 
      field undefined normals <*> 
      field [] faceNormals <*> 
      field [] mapChannels <*> 
      field [] edgeVisibility <*> 
      field [] faceMtlIDs <*> 
      field [] smoothDerivs <*> 
      field False dynamicGeometry
