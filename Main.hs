import Graphics.UI.GLUT as GLUT

import qualified GLUTContext as GLUTC
--import qualified Render
--import qualified Mutate
import RotatingQuad

data GameInput = GameInput
data GameState = GameState


main = do
    Main.initialize
    putStrLn "Intialization done. Entering main loop.."
    mainLoop


initialize :: IO ()
initialize = do
    GLUT.initialize "Super Game" []

    putStrLn "Setting Paramters"
    setGLUTParameters

    putStrLn "Creating Window"
    createWindow "Cool game"

    putStrLn "Setting Rendering Parameters"
    setRenderingParameters

    putStrLn "Creating Context"
    ctx <- GLUTC.make RotatingQuad.transform

    putStrLn "Setting Display Callback"
    displayCallback $= RotatingQuad.display ctx


setGLUTParameters :: IO ()
setGLUTParameters = do
    putStrLn "- Display mode settings"
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    putStrLn "- Window size"
    initialWindowSize  $= Size 640 480


setRenderingParameters :: IO ()
setRenderingParameters = do
    clearColor $= Color4 0 0.0 0 (0 :: GLclampf)
    clear [ColorBuffer]
    drawBuffer         $= BackBuffers
    --viewport               $= ((Position 0 0), Size 640 480)
    --matrixMode             $= Projection
    --loadIdentity
    --perspective 70.0 (640/480) 10.0  4000.0
    matrixMode             $= Modelview 0
    loadIdentity
    --depthFunc              $= Just Less
    --texture Texture2D  $=  Enabled
    --cullFace               $= Just Front
    cursor                 $= None
