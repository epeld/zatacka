module Graphics where
import Prelude hiding (mapM_)
import Data.Foldable

import Control.Lens
import Linear

import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 

import Geometry
import Checkpoint as CP


vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f a b = do 
    vertex $ Vertex2 a b

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

quad :: IO ()
quad = do
    vertex2f (-1) (-1)
    vertex2f (-1) 1
    vertex2f 1 (-1)
    vertex2f 1 1
    vertex2f (-1) 1

lines :: Traversable t => t Checkpoint -> IO ()
lines pts = renderPrimitive LineStrip $ lineVertices pts

lineVertices :: Traversable t => t Checkpoint -> IO ()
lineVertices pts = do
    let ps = fmap (^. CP.position) pts
    let cs = fmap _glcoords ps
    -- mapM_ (putStrLn. show) cs
    mapM_ (uncurry vertex2f) cs


_glcoords :: Geometry.Position FloatType -> (GLfloat, GLfloat)
_glcoords p = (p ^. _coords) & both %~ realToFrac

