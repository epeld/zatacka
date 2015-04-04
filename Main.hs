import Graphics.UI.GLUT as GLUT

import qualified GLUTContext as GLUTC
import Zatacka as Game
import Activate
import Display

data GameInput = GameInput
data GameState = GameState


main = do
    Main.initialize
    putStrLn "Intialization done. Entering main loop.."
    mainLoop


initialize :: IO ()
initialize = do
    GLUT.initialize "Super Game" []

    putStrLn "Setting up Graphics.."
    Display.setup "Zatachoo"

    putStrLn "Creating Context"
    ctx <- context Game.initial

    putStrLn "Setting Display Callback"
    activate ctx Game.transform Game.display


