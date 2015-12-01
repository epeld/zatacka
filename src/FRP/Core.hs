module FRP.Core where
import Prelude hiding (mapM)

import Control.Arrow
import Control.Monad hiding (mapM)

import Pipes
import Pipes.Concurrent

import Data.Traversable


foldp :: Monad m => (a -> s -> s) -> s -> Pipe a s m b
foldp f s = do yield s
               a <- await
               foldp f (f a s)


mailbox :: IO (Consumer a IO (), Producer a IO ())
mailbox = mailbox' unbounded


mailbox' :: Buffer a -> IO (Consumer a IO (), Producer a IO ())
mailbox' b = do
    (output, input) <- spawn b
    return (toOutput output, fromInput input)
        

send :: a -> Consumer a IO () -> IO ()
send x consumer = let eff = yield x >-> consumer in runEffect eff


merge :: Producer a IO () -> Producer a IO () -> IO (Producer a IO ())
merge x y = mergeAll [x, y]


mergeAll :: Traversable t => t (Producer a IO ()) -> IO (Producer a IO ())
mergeAll ps = do
    (consumer, producer) <- mailbox

    mapM (\p -> forkIO (runEffect $ p >-> consumer)) ps

    return producer
