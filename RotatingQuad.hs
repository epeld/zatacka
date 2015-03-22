module RotatingQuad where
import Control.Monad.Trans.Reader

import Transform
import Graphics

transform :: StateTransform Float 
transform dt _ x = asks (+ 2 * pi * 2000 / dt)

display :: GLUTContext Float -> DisplayCallback
display ctx = do
    putStrLn "New frame.."
    clear [ColorBuffer]
    color3f 1.0 1.0 1.0
    rotate 1 $ Vector3 0 0 (1 :: GLfloat)
    renderPrimitive TriangleFan quad
    -- TODO draw some stuff here!
    -- e.g quad
    swapBuffers
