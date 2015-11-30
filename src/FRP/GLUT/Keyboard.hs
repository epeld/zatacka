module Data.Signal.GLUT.Keyboard where
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.Rendering.OpenGL.GL.StateVar 

import Data.Signal

import Data.Set (Set)
import qualified Data.Set as Set


data KeyEvent = Special SpecialKey | Character Char deriving (Show, Eq, Ord)


keyboard :: IO (Producer (Set KeyEvent) m b)
keyboard = do
    up <- keyUp
    down <- keyDown

    keys <- merge (fmap Set.remove up) (fmap Set.insert down)

    return (keys >-> foldp ($) Set.empty)
    

keyUp :: IO (Producer KeyEvent m b)
keyUp = do
    (producer, consumer) <- mailbox

    let up ev _ =  ev `send` consumer

    keyboardUpCallback $= Just (up . Character)
    specialUpCallback $= Just (up . Special)

    return producer


keyDown :: IO (Producer KeyEvent m b)
keyDown = do
    (producer, consumer) <- mailbox

    let down ev _ = ev `send` consumer

    keyboardCallback $= Just (down . Character)
    specialCallback $= Just (down . Special)

    return producer


