module RenderGeometry where
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 

import Data.List

import Linear
import Control.Lens
import Control.Applicative

import Geometry
import Checkpoint
import Combinatorics
import Vertices
import Graphics


render :: Geometry -> IO ()
render g = renderPrimitive Triangles $ do
    let lineData = thickLine 5 (views position0 glcoords g) (views position1 glcoords g)
    --putStrLn $ "Num vertices " ++ show (Data.List.length  lineData)
    lineData & mapMOf_ (each.each) (uncurry vertex2f)

