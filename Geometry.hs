module Geometry where
import Linear hiding (unit)
import Control.Lens

type Position = V2
type Heading = V2

-- calculate the tangent point of a circle
-- meaning the point that a tangent line with the angle 'omega' (relative to heading) were to touch
-- given a circle of radius r
tanpt :: (Floating a, Ord a) => Heading a -> a -> a -> Position a
tanpt h r omega = rvec - (rvec `Geometry.rotate` omega)
    where rvec = r * sign omega *^ uniperp h 
    
uniperp :: Floating a => V2 a -> V2 a
uniperp = unit. perp

unit :: Floating a => Heading a -> Heading a
unit = signorm

rotate :: Floating a => V2 a -> a -> V2 a
rotate v a = rotation a !* v

rotation :: Floating a => a -> M22 a
rotation a = 
    _x._x .~  cos a $
    _x._y .~ -sin a $ 

    _y._x .~  sin a $ 
    _y._y .~  cos a $

    identity

when :: (a -> Bool) -> (a -> a) -> a -> a
when pred f x = if pred x then f x else x

sign :: (Num a, Ord a) => a -> a
sign a = if 0 <= a then 1 else -1

