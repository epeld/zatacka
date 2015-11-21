--
-- Transform state
--
module Transform where
import Control.Monad.Trans.Reader

import Time
import Input

type Transform a = DTime -> [Event] -> Reader a a

runTransform :: Transform a -> DTime -> [Event] -> a -> a
runTransform = fmap $ fmap runReader
