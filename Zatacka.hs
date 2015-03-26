{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Zatacka where

import Transform
import Display
import Graphics
import Input

data Worm = Worm { segments :: [Segment] }
$(makeLenses ''Worm)

data State = State { worm :: Worm, time :: Time }
$(makeLenses ''State)


transform :: Transform State
transform dt input = worm `over` control dt input

display :: Displayer State
display s = do -- TODO render worm!
    return ()



--
-- Helpers
--

head :: Lens [a] (Maybe b) a b
head _ [] = Nothing
head f (x:_) = Just (f x)

