module Data.Signal where
import Control.Arrow
import Control.Monad

import Pipes
import Pipes.Concurrent



foldp :: Monad m => (a -> s -> s) -> s -> Pipe a s m b
foldp f s = do yield s
               a <- await
               foldp f (f a s)


mailbox :: IO (Consumer a IO (), Producer a IO ())
mailbox = do
    (output, input) <- spawn unbounded
    return (toOutput output, fromInput input)
        

send :: a -> Consumer a IO () -> IO ()
send x consumer = let eff = yield x >-> consumer in runEffect eff


merge :: Producer a IO () -> Producer a IO () -> IO (Producer a IO ())
merge x y = do
    (consumer, producer) <- mailbox

    forkIO $ runEffect $ x >-> consumer
    forkIO $ runEffect $ y >-> consumer

    return producer
