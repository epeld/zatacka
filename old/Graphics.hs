module Graphics where
import Prelude hiding (mapM_)
import Data.Foldable

import Control.Lens
import Linear

import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 

import Checkpoint (FloatType)
import Vertices


vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f a b = do 
    vertex $ Vertex2 a b

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b


glcoords :: V2 FloatType -> (GLfloat, GLfloat)
glcoords p = tuplify p & both %~ realToFrac

