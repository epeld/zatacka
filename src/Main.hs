module Main where
import Graphics.UI.GLUT

import FRP.Core
import FRP.GLUT.Time
import FRP.GLUT.Keyboard

import Control.Concurrent hiding (yield)
import Control.Monad
import Data.Monoid

import System.Exit
import System.Mem

import Pipes
import Pipes.Concurrent

import qualified Data.Set as Set


main :: IO ()
main = do
    let keys = Set.fromList [Character 'a', Character 'b']

    getArgsAndInitialize
    window <- createWindow "Hello, World!"
    putStrLn "Hello, World!"

    (consumer, producer) <- mailbox' (latest doNothing)

    let exit = runEffect $ yield exitSuccess >-> consumer

    time <- interval 1000
    forkIO $ runEffect $ for time $ \s ->
        lift $ putStrLn (show (fromIntegral s / fromIntegral (seconds 1)) ++ " seconds passed")

    keyboard <- keyboard
    forkIO $ runEffect $ for keyboard $ \s ->
        lift $ when (Set.isSubsetOf keys s) exit
            

    displayCallback $= return ()

    idleCallback $= Just (idle producer)

    putStrLn "Main loop."
    mainLoop


idle :: Producer (IO ()) IO () -> IO ()
idle producer = do
    runEffect $ producer >-> (do cmd <- await
                                 lift cmd)

doNothing = return ()
