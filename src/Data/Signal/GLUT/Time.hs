module Data.Signal.GLUT.Time where
import Graphics.UI.GLUT.Callbacks.Global (addTimerCallback)
import Graphics.UI.GLUT.State (elapsedTime)
import qualified Graphics.Rendering.OpenGL.GL.StateVar as StateVar

import Data.Signal

timer :: Int -> IO (Signal Int)
timer ms = do
    (signal, update) <- constant undefined

    let runAndReschedule = do
            time <- StateVar.get elapsedTime
            update (const time)
            addTimerCallback ms runAndReschedule

    runAndReschedule

    return signal


