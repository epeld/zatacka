module Main where
import Graphics.UI.GLUT

import Signal


main :: IO ()
main = do
    getArgsAndInitialize
    window <- createWindow "Hello, World!"
    signal <- timerSignal 300
    putStrLn "Hello, World!"
    idleCallback $= Just (runSignal signal)
    displayCallback $= return ()
    putStrLn "Main loop."
    mainLoop


runSignal :: Show a => Signal a -> IO ()
runSignal s = do
    (old, new) <- updateSignal s
    case new of
        Nothing -> return ()
        Just v -> putStrLn (oldNewMessage old v)


oldNewMessage old v = unwords [show old, "->", show v]
