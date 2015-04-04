module Control where
import Prelude hiding (Left, Right)
import Control.Lens

import Input
import Graphics.UI.GLUT
import Direction
import qualified Segment as S
import qualified Worm

change :: [Input.Event] -> Maybe DirectionChange
change input = Nothing

down :: [(Key, DirectionChange)]
down = [
    (Char 'k', Just Left), 
    (Char 'l', Just Right)
    ]

up :: [(Key, DirectionChange)]
up = down & mapped . _2 .~ Nothing 

control dt input = Worm.extend dt (Control.change input)
