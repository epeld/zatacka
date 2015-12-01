module FRP.GLUT.Keyboard where
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.Rendering.OpenGL.GL.StateVar 

import FRP.Core

import Pipes
import Pipes.Prelude as P

import Data.Set (Set)
import qualified Data.Set as Set


data KeyEvent = Special SpecialKey | Character Char deriving (Show, Eq, Ord)


keyboard :: IO (Producer (Set KeyEvent) IO ())
keyboard = do
    up <- keyUp
    down <- keyDown

    keys <- merge (up >-> P.map Set.delete) (down >-> P.map Set.insert)

    return (keys >-> foldp ($) Set.empty)
    

keyUp :: IO (Producer KeyEvent IO ())
keyUp = do
    (consumer, producer) <- mailbox

    let up ev _ =  ev `send` consumer

    keyboardUpCallback $= Just (up . Character)
    specialUpCallback $= Just (up . Special)

    return producer


keyDown :: IO (Producer KeyEvent IO ())
keyDown = do
    (consumer, producer) <- mailbox

    let down ev _ = ev `send` consumer

    keyboardCallback $= Just (down . Character)
    specialCallback $= Just (down . Special)

    return producer


