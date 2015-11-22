module Main where
import Graphics.UI.GLUT

import Data.Signal as Signal
import Data.Signal.GLUT.Time
import Data.Signal.GLUT.Keyboard

import Control.Monad
import System.Exit

import qualified Data.Set as Set


main :: IO ()
main = do
    let keys = Set.fromList [Character 'a', Character 'b']

    getArgsAndInitialize
    window <- createWindow "Hello, World!"
    putStrLn "Hello, World!"

    signal <- timer 1000
    subscribe signal $ \x -> putStrLn "Boink."

    keyboard <- keyboard
    subscribe keyboard $ \s -> 
        when (Set.isSubsetOf keys s) (exitWith ExitSuccess)

    displayCallback $= return ()
    putStrLn "Main loop."
    mainLoop


runSignal :: Show a => Signal a -> IO ()
runSignal s = do
    v <- Signal.get s
    putStrLn (show v)


