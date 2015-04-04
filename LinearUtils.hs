module LinearUtils where
import Linear
import Control.Lens

centre h omega = signum omega *^ uniperp h 
    
uniperp :: Floating a => V2 a -> V2 a
uniperp = signorm. perp

rotation :: Floating a => a -> M22 a
rotation a = 
    _x._x .~  cos a $
    _x._y .~ -sin a $ 

    _y._x .~  sin a $ 
    _y._y .~  cos a $

    identity
