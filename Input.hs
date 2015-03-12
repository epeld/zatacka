module Input where
import Graphics.UI.GLUT (Key, KeyState, Modifiers)
import Graphics.UI.GLUT as GLUT

data InputEvent = InputEvent Key KeyState Modifiers GLUT.Position deriving (Show, Eq)
