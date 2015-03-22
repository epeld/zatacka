module Render where
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 
import Graphics

import GLUTContext as GLUTC

type StateRenderer a = Reader a (IO ())

display :: GLUTContext a -> StateRenderer a -> DisplayCallback
display ctx r = runReader r $ state ctx

display :: GLUTContext -> DisplayCallback
display ctx = do
    putStrLn "New frame.."
    clear [ColorBuffer]
    color3f 1.0 1.0 1.0
    renderPrimitive TriangleFan quad
    -- TODO draw some stuff here!
    -- e.g quad
    swapBuffers

