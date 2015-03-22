module Input where
import Data.IORef (IORef)

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (Key, KeyState, Modifiers, Position, leaveMainLoop)

data InputEvent = InputEvent Key KeyState Modifiers GLUT.Position deriving (Show, Eq)

type InputIORef = IORef [InputEvent]

handleSpecial :: [InputEvent] -> IO ()
handleSpecial [] = leaveMainLoop
handleSpecial xs = return ()

keyboardMouse :: InputIORef -> KeyboardMouseCallback
keyboardMouse iorf key keyState mods pos = modifyIORef' iorf ( event : )
    where event = InputEvent key keyState mods pos 
