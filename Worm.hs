{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Worm where
import Data.Maybe

import Transform
import Display
import Graphics
import Input
import Segment

data Worm = Worm { segments :: [Segment], initial :: Checkpoint } deriving (Show, Eq)
$(makeLenses ''Worm)

