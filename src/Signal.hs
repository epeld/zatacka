module Signal where
import Graphics.UI.GLUT.Callbacks.Global (addTimerCallback)
import Graphics.UI.GLUT.State (elapsedTime)
import Graphics.Rendering.OpenGL.GL.StateVar (get)

import Data.IORef

data Signal a = Signal { updateSignal :: IO (a, Maybe a) }


timerSignal :: Int -> IO (Signal Int)
timerSignal ms = do
    ref <- newIORef 0

    let runAndReschedule :: IO ()
        runAndReschedule = do 
            time <- get elapsedTime
            writeIORef ref time
            addTimerCallback ms runAndReschedule 

    let update :: IO (Int, Maybe Int)
        update = do
            time <- readIORef ref
            return (time, Just time)

    runAndReschedule

    return (Signal update)
        
