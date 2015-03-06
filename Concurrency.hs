module Concurrency where
import Control.Arrow ((&&&))

import Data.IORef

writeIORef r v = atomicModifyIORef' r (const v &&& id) 
