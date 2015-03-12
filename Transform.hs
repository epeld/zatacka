module Transform where
import Time
import Input

type StateTransform a = DTime -> [InputEvent] -> a -> a
