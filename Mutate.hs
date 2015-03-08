module Mutate where
import Control.Monad (when)
import Graphics.UI.GLUT as GLUT
import Data.IORef (modifyIORef')

import Game

type StateMutator = DTime -> [InputEvent] -> IO ()

game :: IORef Game.State -> StateMutator
game ior dt is = modifyIORef' ior (nextState dt is)

mutator :: StateMutator
mutator delta input = do
    putStrLn $ unwords ["elapsed since last tick", show delta]
    when (input /= []) leaveMainLoop


