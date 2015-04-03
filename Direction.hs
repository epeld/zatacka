module Direction where

data Direction = Left | Right deriving (Show, Eq)

type DirectionChange = Maybe Direction
