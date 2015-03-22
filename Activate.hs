module Activate where
import GLUTContext
import Transform

import Control.Lens

activate :: GLUTContext a -> StateTransform a -> IO ()
activate c t = do
    activateIdle c t
    activateInput c
    
activateIdle :: GLUTContext a -> StateTransform a -> IO ()
activateIdle c t = idle $= Idle.idle c t

activateInput :: GLUTContext a -> IO ()
acticateInput c = keyboardMouseCallback $= c ^. inputRef . to Input.keyboardMouse

context :: a -> IO (GLUTContext a)
context init = pure GLUTContext <*> newIORef [] <*> newTimeIORef <*> newIORef a
