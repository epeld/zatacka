module Data.Signal.GLUT.Keyboard where
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.Rendering.OpenGL.GL.StateVar 

import Data.Signal

import Data.Set (Set)
import qualified Data.Set as Set

data KeyEvent = Special SpecialKey | Character Char deriving (Show, Eq, Ord)

keyboard :: IO (Signal (Set KeyEvent))
keyboard = do
    (signal, update) <- constant Set.empty
    
    up <- keyUp
    down <- keyDown

    subscribe down (update . Set.insert)
    subscribe up (update . Set.delete)

    return signal


keyUp :: IO (Signal KeyEvent)
keyUp = do
    (signal, update) <- constant (Special KeyBegin)

    let up ev _ = update (const ev)

    keyboardUpCallback $= Just (up . Character)
    specialUpCallback $= Just (up . Special)

    return signal


keyDown :: IO (Signal KeyEvent)
keyDown = do
    (signal, update) <- constant (Special KeyBegin)

    let down ev _ = update (const ev)

    keyboardCallback $= Just (down . Character)
    specialCallback $= Just (down . Special)

    return signal
