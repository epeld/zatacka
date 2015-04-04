module NonEmptyUtil where
import Control.Lens hiding (_head)
import Control.Applicative

import Data.List.NonEmpty as NonEmpty

_head :: Lens' (NonEmpty a) a
_head f (a :| as) = (:| as) <$> f a

_tail :: Lens' (NonEmpty a) [a]
_tail f (a :| as) = (a :|) <$> f as
