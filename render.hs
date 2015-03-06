module Render where
import Graphics.UI.GLUT (swapBuffers, DisplayCallback)
import Graphics.Rendering.OpenGL (clear, ClearBuffer(..))


display :: DisplayCallback
display = do
    clear [ColorBuffer, DepthBuffer]
    swapBuffers

