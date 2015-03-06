module GLUTContext where
import Prelude as P

import System.IO (IO)

import Control.Monad ((>>=), (=<<), forM_, return, (>=>))
import Control.Applicative
import Control.Arrow ((&&&))

import Data.IORef (atomicModifyIORef, atomicModifyIORef', modifyIORef', readIORef, IORef, newIORef)
import Data.Function (id, const)
import Data.Functor (fmap)

import Graphics.UI.GLUT (IdleCallback, KeyboardMouseCallback, elapsedTime,
                         idleCallback, keyboardMouseCallback,
                         Key, KeyState, Modifiers, Position,
                         get, ($=),
                         swapBuffers)

import Concurrency (writeIORef)

type Time = Int
type DTime = Int

type StateMutator = DTime -> [InputEvent] -> IO ()

data InputEvent = InputEvent Key KeyState Modifiers Position deriving (Show, Eq)

type TimeIORef = IORef Time
type InputIORef = IORef [InputEvent]

data GLUTContext = GLUTContext { inputRef :: InputIORef, timeRef :: TimeIORef, mutator :: StateMutator }

runMutator :: StateMutator -> IO ()
runMutator = context >=> activate

activate :: GLUTContext -> IO ()
activate c = do
    idleCallback $= Just (idle c)
    keyboardMouseCallback $= Just (keyboardMouse c)
    
context :: StateMutator -> IO GLUTContext
context m = pure GLUTContext <*> newIORef [] <*> newTimeIORef <*> pure m

newTimeIORef :: IO TimeIORef
newTimeIORef = newIORef =<< get elapsedTime

keyboardMouse :: GLUTContext -> KeyboardMouseCallback
keyboardMouse c = keyboardMouse' (inputRef c)

keyboardMouse' :: InputIORef -> KeyboardMouseCallback
keyboardMouse' iorf key keyState mods pos = modifyIORef' iorf ( event : )
    where event = InputEvent key keyState mods pos 


idle :: GLUTContext -> IdleCallback
idle c = do
    delta <- newTimeDelta c 
    inputs <- gatherPendingInput c
    mutator c delta inputs


gatherPendingInput :: GLUTContext -> IO [InputEvent]
gatherPendingInput c = writeIORef (inputRef c) []


newTimeDelta :: GLUTContext -> IO DTime
newTimeDelta c = do
    currentTime <- fromIntegral `fmap` get elapsedTime
    lastTime <- writeIORef (timeRef c) currentTime
    return $ currentTime - lastTime

