module Mutate where
import Control.Monad (when)
import Graphics.UI.GLUT as GLUT
import Data.IORef (modifyIORef')
import Data.IORef

import Input
import Time
import Transform

type StateMutator a = DTime -> [InputEvent] -> IO a

mutator :: StateMutator ()
mutator delta [] = do
    delta `seq` return ()
    --putStrLn $ unwords ["elapsed since last tick", show delta]

mutator delta input = do
    putStrLn (show input)
    leaveMainLoop

