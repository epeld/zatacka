module Idle where
import System.IO (IO)

import Data.IORef (readIORef, IORef, newIORef, modifyIORef')

import Control.Monad

import GLUTContext
import Transform
import qualified Time
import qualified Input
import Concurrency (writeIORef)

idle :: GLUTContext a -> StateTransform a -> IdleCallback
idle c t = do
    delta <- newTimeDelta c 
    inputs <- gatherPendingInput c
    Input.handleSpecial inputs
    mutateState c t

mutateState :: GLUTContext a -> StateTransform a -> IO ()
mutateState c t = modifyIORef' (runTransform t delta inputs) (stateRef c)

newTimeDelta :: GLUTContext a -> IO DTime
newTimeDelta = Time.newTimeDelta. timeRef

gatherPendingInput :: GLUTContext a -> IO [InputEvent]
gatherPendingInput c = writeIORef (inputRef c) []
