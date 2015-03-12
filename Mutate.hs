module Mutate where
import Control.Monad (when)
import Graphics.UI.GLUT as GLUT
import Data.IORef (modifyIORef')
import Data.IORef

import Input
import Time
import Transform

type StateMutator a = DTime -> [InputEvent] -> IO a

mutator :: StateMutator a
mutator _ [] = do
    putStrLn $ unwords ["elapsed since last tick", show delta]

mutator delta input = do
    leaveMainLoop



-- Construct a state mutator from a pure state transform
make :: a -> StateTransform a -> StateMutator a
make initial f =
    let s = newIORef initial
    in \dt input -> modifyIORef' s (f dt input)
