module Main where
import Graphics.UI.GLUT

import Signal


main :: IO ()
main = do
    getArgsAndInitialize
    window <- createWindow "Hello, World!"
    signal <- timer 10
    putStrLn "Hello, World!"
    subscribe signal $ \x -> putStrLn (show x)
    displayCallback $= return ()
    putStrLn "Main loop."
    mainLoop


runSignal :: Show a => Signal a -> IO ()
runSignal s = do
    v <- Signal.get s
    putStrLn (show v)


