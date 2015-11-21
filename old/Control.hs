module Control where
import Prelude hiding (Left, Right)
import Control.Lens

import Input
import Graphics.UI.GLUT
import Direction

type DirectionChange = Maybe Turn

change :: [Input.Event] -> Maybe DirectionChange
change [] = Nothing
change (x:_) = case x ^. keyState of
    Down -> lookup k downkeys
    Up -> lookup k upkeys
    where
    downkeys = keys & mapped._2 %~ Just
    upkeys = keys & mapped._2 .~ Nothing
    k = x ^. key

keys :: [(Key, Turn)]
keys = [
    (Char 'a', Left),
    (Char 's', Right),
    (SpecialKey KeyLeft, Left),
    (SpecialKey KeyRight, Right)]


up :: [Key]
up = fmap fst keys
