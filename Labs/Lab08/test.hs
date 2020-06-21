import Data.List
import Data.Maybe


-- getFirst pair@(P firstValue secondValue) = firstValue pair
data T a = C {field :: a}
data Lst a = Nil | Cons a (Lst a)
