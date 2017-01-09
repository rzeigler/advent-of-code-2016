module Day2 
  ( RangeConstraint(..)
  , Constraints(..)
  , inRange
  , isConstrained
  ) where 

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Foldable (foldl)
import Control.Monad.RWS (RWS)

data RangeConstraint = RangeConstraint (Int -> Int) Ordering
data Constraints = Constraints Int Int (List RangeConstraint)

inRange :: (Tuple Int Int) -> RangeConstraint -> Boolean
inRange (Tuple x y) (RangeConstraint f ord) = 
  let rel = compare (f x) y
  in rel == EQ || rel == ord

isConstrained :: Tuple Int Int -> Constraints -> Boolean
isConstrained pt@(Tuple x y) (Constraints xMin xMax rs) = 
  x >= xMin && x <= xMax && foldl (&&) true (map (inRange pt) rs)

-- Note that this is inverted visual along the y axis in order to conform to the expected orientation where 
-- [0, 0] is the bottom left button on a physical keypad
grid1 :: Array (Array Char)
grid1 = [
  ['1', '2', '3'],
  ['4', '5', '6'],
  ['7', '8', '9']
]

-- grid2 :: Array (Array Char)

