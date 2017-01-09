module Day2 
  ( RangeConstraint(..)
  , Constraints(..)
  , Vec2(..)
  , inRange
  , isConstrained
  ) where 

import Prelude
import Control.Monad (pure)
import Control.Monad.RWS (RWS, tell, get, put)
import Control.Monad.Reader (Reader(..), ReaderT(..), ask, runReader)
import Data.Array (index, (!!))
import Data.Foldable (foldl)
import Data.List (List)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Util (unwrap)

data RangeConstraint = RangeConstraint (Int -> Int) Ordering
data Constraints = Constraints Int Int (List RangeConstraint)
data Vec2 = Vec2 Int Int
instance vec2Show :: Show Vec2 where
  show (Vec2 x y) = "(" <> show x <> "," <> show y <> ")"
instance vec2Semigroup :: Semigroup Vec2 where
  append (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

inRange :: Vec2 -> RangeConstraint -> Boolean
inRange (Vec2 x y) (RangeConstraint f ord) =
  let rel = compare (f x) y
  in rel == EQ || rel == ord

isConstrained :: Vec2 -> Constraints -> Boolean
isConstrained pt@(Vec2 x y) (Constraints xMin xMax rs) =
  (x >= xMin && x <= xMax && foldl (&&) true (map (inRange pt) rs))

data Dir = U | D | L | R
instance dirShow :: Show Dir where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"

delta :: Dir -> Vec2
delta U = Vec2 0 1
delta D = Vec2 0 (-1)
delta R = Vec2 1 0
delta L = Vec2 (-1) 0

-- Note that this is inverted visual along the y axis in order to conform to the expected orientation where 
-- [0, 0] is the bottom left button on a physical keypad
grid1 :: Array (Array Char)
grid1 = [
  ['1', '2', '3'],
  ['4', '5', '6'],
  ['7', '8', '9']
]

data Env = Env Constraints (Array (Array Char))

deref :: Vec2 -> RWS (Array (Array Char)) (Array String) Vec2 Char
deref (Vec2 x y) = do
  grid <- ask
  pure (unsafePartial $ unwrap $ (flip index) x =<< (grid !! y))
