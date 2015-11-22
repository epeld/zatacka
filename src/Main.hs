module Main where
import Graphics.UI.GLUT

import Data.Signal as Signal
import Data.Signal.GLUT.Time
import Data.Signal.GLUT.Keyboard

import Control.Monad


main :: IO ()
main = do
    getArgsAndInitialize
    window <- createWindow "Hello, World!"
    signal <- foldp (:) [] =<< timer 500
    putStrLn "Hello, World!"
    subscribe signal $ \x -> putStrLn (show x)
    displayCallback $= return ()
    putStrLn "Main loop."
    mainLoop


runSignal :: Show a => Signal a -> IO ()
runSignal s = do
    v <- Signal.get s
    putStrLn (show v)


