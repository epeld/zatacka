{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GLUTContext where
import Prelude as P

import System.IO (IO)

import Control.Lens

import Data.IORef

import Time
import Input

type StateRef a = IORef a

data GLUTContext a = GLUTContext {
    _inputRef :: InputIORef, 
    _timeRef :: TimeIORef, 
    _stateRef :: StateRef a }

$(makeLenses ''GLUTContext)


state :: GLUTContext a -> IO a
state = (^. stateRef . to readIORef)
