{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Zatacka where
import Control.Lens
import Control.Monad.Trans.Reader

import Graphics.UI.GLUT as GLUT

import Linear

import Time
import Transform
import Segment
import Display
import Graphics
import Input
import Control
import Checkpoint
import qualified Worm 

data State = State { _worm :: Worm.Worm, _time :: Time }
$(makeLenses ''State)

initial :: State
initial = State { _worm = w, _time = 0 }
    where w = Worm.worm cp
          cp = Checkpoint (V2 100 100) (V2 50 0)

transform :: Transform State
transform dt input = do
    s <- ask
    let s' = s & worm %~ Worm.extend dt (Control.change input) & time +~ dt
    return s'


display :: Displayer State
display s = do 
    clear [ColorBuffer]
    color3f 1.0 0.5 0
    renderWorm (s ^. worm)
    swapBuffers


renderWorm :: Worm.Worm -> IO ()
renderWorm w = Graphics.lines (w ^. to interp)
    where interp = Worm.checkpoints 0.3
    


putInfo :: State -> IO ()
putInfo s = do
    let w = s ^. worm 
        cps = Worm.checkpoints 0.3 w
        t = s ^. time
    putStrLn "Info:"
    putStrLn ("Time: " ++ (show $ t))
    putStrLn ("Worm: " ++ (show w))
    putStrLn ("Worm time: " ++ (show $ Worm.endTime w))
    putStrLn (show cps)
