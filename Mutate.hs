module Mutate where
import Control.Monad (when)
import Graphics.UI.GLUT as GLUT

data Position = Vector3 Int Int Int


mutator delta input = do
    putStrLn $ unwords ["elapsed since last tick", show delta]
    when (input /= []) leaveMainLoop
