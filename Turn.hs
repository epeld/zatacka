module Turn where
import Geometry

data Turn = Turn { direction :: Direction, start :: Time, end :: Time } deriving (Show, Eq)

data State = State { position :: Position, heading :; Heading } deriving (Show, Eq)

data Direction = Left | Right deriving (Show, Eq)

type StateMutator = State -> State

state :: [Turn] -> Time -> StateMutator
state [] t = extrapolate t
state (trn : turns) t = 
    state turns (t - trnstop).                 -- :: StateMutator rest
    turn (direction trn) (trnstop - trnstart). -- :: StateMutator turn
    extrapolate trnstart                       -- :: StateMutator before
    where
    trnstart = min (start trn) t
    trnstop  = max (end trn) t
    t' = t - trnstop
        

-- extrapolate from t = 0 to the given time
extrapolate :: Time -> StateMutator
extrapolate t s = position s ^+^ t *^ heading s

-- extrapolate a turn from t = 0 to the given time
turn :: Direction -> Time -> StateMutator
turn d t s = State p' h'
    where h' = heading s `rotate` omega
          p' = tanpt p r omega
          r = speed s / omega
          omega = angular d * t

angular :: Floating => Direction -> a
angular Left = - pi / 8
angular Right = -angular Left
