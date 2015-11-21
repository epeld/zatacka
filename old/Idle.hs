{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Idle where
import System.IO (IO)

import Data.IORef (readIORef, IORef, newIORef, modifyIORef')

import Control.Monad
import Control.Lens

import Graphics.UI.GLUT (IdleCallback, postRedisplay)

import GLUTContext
import Transform
import qualified Time
import qualified Input
import Concurrency (writeIORef)

callback :: GLUTContext a -> Transform a -> IdleCallback
callback c t = do
    delta <- newTimeDelta c 
    inputs <- gatherPendingInput c
    Input.handleSpecial inputs
    mutateState c (runTransform t delta inputs)
    postRedisplay Nothing

mutateState :: GLUTContext a -> (a -> a) -> IO ()
mutateState c f = modifyIORef' (c ^. stateRef) f

newTimeDelta :: GLUTContext a -> IO Time.DTime
newTimeDelta c = Time.newTimeDelta (c ^. timeRef)

gatherPendingInput :: GLUTContext a -> IO [Input.Event]
gatherPendingInput c = writeIORef (c ^. inputRef) []
