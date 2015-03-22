module Activate where
import GLUTContext
import Transform

import qualified Display
import qualified Input
import qualified Idle
import qualified Time

import Control.Lens
import Control.Applicative

import Data.IORef

import Graphics.UI.GLUT

activate :: GLUTContext a -> Transform a -> Display.Displayer a -> IO ()
activate c t d = do
    activateInput c
    activateIdle c t
    activateDisplay c d

activateDisplay :: GLUTContext a -> Display.Displayer a -> IO ()
activateDisplay c d = displayCallback $= Display.callback c d
    
activateIdle :: GLUTContext a -> Transform a -> IO ()
activateIdle c t = idleCallback $= Just (Idle.callback c t)

activateInput :: GLUTContext a -> IO ()
activateInput c = keyboardMouseCallback $= c ^. inputRef . to Input.callback. to Just

context :: a -> IO (GLUTContext a)
context init = pure GLUTContext <*> newIORef [] <*> Time.newTimeIORef <*> newIORef init
