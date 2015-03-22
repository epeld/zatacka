module Transform where
import Control.Monad.Trans.Reader

import Time
import Input

type StateTransform a = DTime -> [InputEvent] -> Reader a a

runTransform :: StateTransform a -> DTime -> [InputEvent] -> a -> a
runTransform = fmap $ fmap runReader
