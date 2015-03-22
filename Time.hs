module Time where
import Data.IORef (readIORef, IORef, newIORef, modifyIORef')

import Control.Monad

import qualified Graphics.UI.GLUT as GLUT

import Concurrency (writeIORef)

type FloatType = Float

type Time = FloatType
type DTime = FloatType

type TimeIORef = IORef Time

newTimeIORef :: IO TimeIORef
newTimeIORef = newIORef =<< elapsedTime

elapsedTime :: IO Time
elapsedTime = do
    ms <- GLUT.get GLUT.elapsedTime
    return $ fromIntegral ms / 1000


newTimeDelta :: TimeIORef -> IO DTime
newTimeDelta t = do
    currentTime <- elapsedTime
    lastTime <- writeIORef t currentTime
    return $ currentTime - lastTime

