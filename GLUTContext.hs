module GLUTContext where
import Prelude as P

import System.IO (IO)

import Control.Monad ((>>=), (=<<), forM_, return, (>=>), (<=<))
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
import Time
import Input

import qualified Mutate
import qualified Transform

type TimeIORef = IORef Time
type InputIORef = IORef [InputEvent]

data GLUTContext a = GLUTContext {
    inputRef :: InputIORef, 
    timeRef :: TimeIORef, 
    mutator :: Mutate.StateMutator a }

-- attach an adapter, e.g for drawing graphics, on top of a context
attachIO = flip withIO

-- guarantee completely IMpure side-effects, meaning
-- they do not modify the value in any way. They ONLY cause side effects
-- example: attachIO (sideEffect Graphics.draw) ctx
sideEffect :: (a -> IO ()) -> a -> IO a
sideEffect f a = f a >> return a

withIO :: GLUTContext a -> (a -> IO a) -> GLUTContext a
withIO c f = c { mutator = fmap (f <=<) (mutator c) }

make :: Transform.StateTransform a -> a -> IO (GLUTContext a)
make f initial = do
    s <- newIORef initial
    let m dt input = atomicModifyIORef' s (f dt input &&& id)
    context m

activate :: GLUTContext a -> IO ()
activate c = do
    idleCallback $= Just (idle c)
    keyboardMouseCallback $= Just (keyboardMouse c)
    
context :: Mutate.StateMutator a -> IO (GLUTContext a)
context m = pure GLUTContext <*> newIORef [] <*> newTimeIORef <*> pure m

newTimeIORef :: IO TimeIORef
newTimeIORef = newIORef =<< get elapsedTime

keyboardMouse :: GLUTContext a -> KeyboardMouseCallback
keyboardMouse c = keyboardMouse' (inputRef c)

keyboardMouse' :: InputIORef -> KeyboardMouseCallback
keyboardMouse' iorf key keyState mods pos = modifyIORef' iorf ( event : )
    where event = InputEvent key keyState mods pos 


idle :: GLUTContext a -> IdleCallback
idle c = do
    delta <- newTimeDelta c 
    inputs <- gatherPendingInput c
    mutator c delta inputs
    return ()


gatherPendingInput :: GLUTContext a -> IO [InputEvent]
gatherPendingInput c = writeIORef (inputRef c) []


newTimeDelta :: GLUTContext a -> IO DTime
newTimeDelta c = do
    currentTime <- pure fromIntegral <*> get elapsedTime
    lastTime <- writeIORef (timeRef c) currentTime
    return $ currentTime - lastTime

