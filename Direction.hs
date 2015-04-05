module Direction where
import Prelude hiding (Left, Right)

data Direction = Left | Right deriving (Show, Eq)

sign Right = -1
sign Left = 1
