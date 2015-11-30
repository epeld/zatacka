module Scene where

data Camera = Camera { } deriving (Show, Eq)

data Scene = Scene { camera :: Camera } deriving (Show, Eq)
