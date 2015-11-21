module Display (callback, Displayer, setup) where
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL 

import GLUTContext as GLUTC

type Displayer a = a -> IO ()

callback :: GLUTContext a -> Displayer a -> DisplayCallback
callback ctx d = state ctx >>= runDisplayer d

runDisplayer :: Displayer a -> a -> IO ()
runDisplayer = ($)


setup :: String -> IO ()
setup title = do
    setGLUTParameters
    createWindow title
    setGLParameters

setGLParameters :: IO ()
setGLParameters = do
    clearColor $= Color4 0 0.0 0 (0 :: GLclampf)
    clear [ColorBuffer]
    drawBuffer             $= BackBuffers
    viewport               $= ((Position 0 0), Size 640 480)
    matrixMode             $= Projection
    --loadIdentity
    --perspective 70.0 (640/480) 10.0  4000.0
    ortho (-500) 500 (-500) 500 (-500) 500
    matrixMode             $= Modelview 0
    loadIdentity
    cursor                 $= None

setGLUTParameters :: IO ()
setGLUTParameters = do
    putStrLn "- Display mode settings"
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    putStrLn "- Window size"
    initialWindowSize  $= Size 640 480

