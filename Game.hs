module Game where
import Linear

import Time
import Input
import Transform

data GameState a = GameState { worms :: [Worm a] }

data Worm a = Worm

type Position = V2

-- nextState :: State -> DTime -> [InputEvent] -> State
-- nextState s dt is = s -- TODO
