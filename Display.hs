module Display (callback, Displayer) where
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 

import GLUTContext as GLUTC

type Displayer a = a -> IO ()

callback :: GLUTContext a -> Displayer a -> DisplayCallback
callback ctx d = state ctx >>= runDisplayer d

runDisplayer :: Displayer a -> a -> IO ()
runDisplayer = ($)
