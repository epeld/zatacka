module Data.Signal.GLUT.Time where
import Graphics.UI.GLUT.Callbacks.Global (addTimerCallback)
import Graphics.UI.GLUT.State (elapsedTime)
import qualified Graphics.Rendering.OpenGL.GL.StateVar 

import Data.Signal

type Time = Int


hours :: Int -> Time
hours x = x * minutes 60


minutes :: Int -> Time
minutes x = x * seconds 60


seconds :: Int -> Time
seconds x = x * milliseconds 1000


milliseconds :: Int -> Time
milliseconds x = x


interval :: Time -> IO (Producer Int IO ())
interval t = do
    (consumer, producer) <- mailbox

    repeated (addTimerCallback t) $ do
        time <- get elapsedTime
        time `send` consumer

    return producer


repeated :: (IO () -> IO ()) -> IO () -> IO ()
repeated redo action = do action
                          redo (repeated redo action)

