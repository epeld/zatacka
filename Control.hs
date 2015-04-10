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
    Down -> Just $ lookup k keys
    Up -> lookup k upkeys
    where
    upkeys = zip up $ repeat Nothing
    k = x ^. key

keys :: [(Key, Turn)]
keys = [
    (Char 'a', Left),
    (Char 's', Right),
    (SpecialKey KeyLeft, Left),
    (SpecialKey KeyRight, Right)]


up :: [Key]
up = fmap fst keys
