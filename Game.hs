module Game where
import Time

data InputEvent = InputEvent Key KeyState Modifiers Position deriving (Show, Eq)

data GameState = { worms :: [Worm] }

data Worm = [Vector3]

data Position = Vector3 Int Int Int


nextState :: State -> DTime -> [InputEvent] -> State
nextState s dt is = s -- TODO
