{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Zatacka where
import Control.Lens
import Control.Applicative
import Control.Monad.Trans.Reader

import Graphics.UI.GLUT as GLUT

import Linear

import Time
import Transform
import Display
import Graphics
import Input
import Control
import Checkpoint
import TimeSeries
import TimeSeriesGeometry
import Player as P
import RenderGeometry as RG

data State = State { _player1 :: Player, _time :: Time }
$(makeLenses ''State)

transform :: Transform State
transform dt input = do
    s <- ask
    let ev = TimeSeries.Event <$> Control.change input <*> pure dt
    s & player1 %~ P.change dt ev & return


display :: Displayer State
display s = do 
    clear [ColorBuffer]
    color3f 1.0 0.5 0

    -- putStrLn (show $ view (player1.timeseries.events) s)

    let geo = geometries (s ^. player1.timeseries.events. to reverse) (s ^. player1. P.initial)
    -- putStrLn $ show $ length geo

    mapM_ RG.render geo
    swapBuffers



initial :: State
initial = State { _player1 = p, _time = 0 }
    where p = P.player (V2 100 100) (V2 10 0)

