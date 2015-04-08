module Control where
import Prelude hiding (Left, Right)
import Control.Lens

import Input
import Graphics.UI.GLUT
import Direction

change :: [Input.Event] -> (Maybe Turn)
change input = Nothing

down :: [(Key, Maybe Turn)]
down = []
--    (Char 'k', Just Left), 
--    (Char 'l', Just Right)
--    ]

up :: [(Key, Maybe Turn)]
up = down & mapped . _2 .~ Nothing 
