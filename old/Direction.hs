module Direction where
import Prelude hiding (Left, Right)

data Direction = Left | Right deriving (Show, Eq)
type Turn = Direction

sign Right = 1
sign Left = -1
