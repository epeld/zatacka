module Data.Signal where
import Control.Arrow
import Control.Monad

import Data.IORef

type Listener a = a -> IO ()

type UpdateFn a = (a -> a) -> IO ()

data Signal a = Signal { get :: IO a, subscribe :: Listener a -> IO () }


instance Functor Signal where
    fmap f s = Signal (fmap f (get s)) (\l -> subscribe s (l . f))


atomicCons ref x = atomicModifyIORef ref (\xs -> (x : xs, xs))
atomicCons_ ref x = atomicModifyIORef ref (\xs -> (x : xs, ()))


constant :: a -> IO (Signal a, UpdateFn a)
constant initial = do
    ref <- newIORef initial
    lref <- newIORef []

    let updates f = do
            v <- atomicModifyIORef ref (f &&& f)
            listeners <- readIORef lref
            mapM_ ($ v) listeners

    let gets = readIORef ref
    let subscribes = atomicCons_ lref

    return (Signal gets subscribes, updates)
        

delayed :: Int -> Signal a -> IO (Signal a)
delayed n s = do
    v <- get s

    (signal, update) <- constant v
    ref <- newIORef []

    subscribe s $ \a -> do
        v <- atomicModifyIORef ref $ \as -> 
            let
                as' = take n (a : as)
            in
                (as', nth n as')

        maybe (return ()) (update . const) v
       
    return signal


foldp :: (a -> s -> s) -> s -> Signal a -> IO (Signal s)
foldp f s as = do
    (signal, update) <- constant s

    subscribe as $ \a -> update (f a)

    return signal


nth _ [] = Nothing
nth 0 [x] = Just x
nth n (x : xs) = nth (n-1) xs
