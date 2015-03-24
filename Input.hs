module Input where
import Data.IORef (IORef, modifyIORef')

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (Key(..), KeyState(..), Modifiers, Position, 
                         leaveMainLoop, KeyboardMouseCallback)

data Event = Event Key KeyState Modifiers GLUT.Position deriving (Show, Eq)

type InputIORef = IORef [Event]

handleSpecial :: [Event] -> IO ()
handleSpecial [] = return ()
handleSpecial xs = mapM_ handleSpecial' xs
    where
    handleSpecial' (Event (Char 'q') Down _ _) = leaveMainLoop
    handleSpecial' x = putStrLn (show x) >> return ()

callback :: InputIORef -> KeyboardMouseCallback
callback iorf key keyState mods pos = modifyIORef' iorf ( event : )
    where event = Event key keyState mods pos 

