module Signal where
import Graphics.UI.GLUT.Callbacks.Global (addTimerCallback)
import Graphics.UI.GLUT.State (elapsedTime)
import qualified Graphics.Rendering.OpenGL.GL.StateVar as StateVar

import Control.Monad
import Data.IORef

type Listener a = a -> IO ()

data Signal a = Signal { get :: IO a, set :: a -> IO (), subscribe :: Listener a -> IO () }


atomicCons ref x = atomicModifyIORef ref (\xs -> (x : xs, xs))
atomicCons_ ref x = atomicModifyIORef ref (\xs -> (x : xs, ()))


constant :: a -> IO (Signal a)
constant initial = do
    ref <- newIORef initial
    lref <- newIORef []

    let sets a = writeIORef ref a >> readIORef lref >>= mapM_ ($ a) 
    let gets = readIORef ref
    let subscribes = atomicCons_ lref

    return (Signal gets sets subscribes)
        

timer :: Int -> IO (Signal Int)
timer ms = do
    signal <- constant 0

    let runAndReschedule = do
            time <- StateVar.get elapsedTime
            set signal time
            addTimerCallback ms runAndReschedule

    runAndReschedule

    return signal


delayed :: Int -> Signal a -> IO (Signal a)
delayed n s = do
    v <- get s

    signal <- constant v
    ref <- newIORef []

    subscribe s $ \a -> do
        v <- atomicModifyIORef ref $ \as -> 
            let
                as' = take n (a : as)
            in
                (as', nth n as')

        case v of
            Just val -> set signal val
            Nothing -> return ()
       
    return signal


nth _ [] = Nothing
nth 0 [x] = Just x
nth n (x : xs) = nth (n-1) xs
