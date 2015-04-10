{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Input where
import Control.Lens

import Data.IORef (IORef, modifyIORef')

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (Key(..), KeyState(..), Modifiers, Position, 
                         leaveMainLoop, KeyboardMouseCallback)

data Event = Event {
    _key :: Key,
    _keyState :: KeyState,
    _modifiers :: Modifiers,
    _position :: GLUT.Position } deriving (Show, Eq)

makeLenses ''Event

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

