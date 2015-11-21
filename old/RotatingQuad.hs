module RotatingQuad where
import Control.Monad.Trans.Reader
import Control.Monad

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL

import Transform
import Graphics
import Display

type State = Float

transform :: Transform State
transform dt _ = asks $ (+ 180 * dt) . currentAngle

display :: Displayer State
display angle = do
    clear [ColorBuffer]
    color3f 1.0 1.0 1.0
    preservingMatrix $ do
        scale 10 10 (10 :: GLfloat)
        RotatingQuad.rotate $ realToFrac angle
        renderPrimitive TriangleFan quad
    swapBuffers

rotate :: GLfloat -> IO ()
rotate = flip GLUT.rotate $ Vector3 0 0 (1 :: GLfloat)

currentAngle :: State -> Float
currentAngle = id
