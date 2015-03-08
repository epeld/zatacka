module Worm where
import Geometry
import Turn

data Worm = Worm { turns :: [Turn], initial :: State } deriving (Show, Eq)

states :: Functor f => Worm -> f Time -> f State
states w = fmap (state w)

state :: Worm -> Time -> State
state (Worm turns s) = Geometry.state turns s

