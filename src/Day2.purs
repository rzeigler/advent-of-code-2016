module Day2 
  ( RangeConstraint(..)
  , Constraints(..)
  , Vec2(..)
  , Dir(..)
  , Env
  , inRange
  , isConstrained
  , env1
  , move
  , line
  , file
  ) where

import Prelude
import Control.Monad.RWS (RWS, get, put, tell, ask)
import Data.Array (index, (!!))
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Util (unwrap)

data RangeConstraint = RangeConstraint (Int -> Int) Ordering
data Constraints = Constraints Int Int (Array RangeConstraint)
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

constraint1 :: Constraints
constraint1 = Constraints 0 2 [RangeConstraint (const 2) GT, RangeConstraint (const 0) LT]

data Env = Env Constraints (Array (Array Char))

env1 :: Env
env1 = Env constraint1 grid1

type PadRWS = RWS Env (Array String) Vec2

deref :: PadRWS Char
deref = do
  v2@(Vec2 x y) <- get
  (Env _ grid) <- ask
  tell ["position is now " <> show v2]
  pure (unsafePartial $ unwrap $ (flip index) x =<< (grid !! y))

move :: Dir -> PadRWS Char
move dir = do
  tell ["taking action " <> show dir]
  old <- get
  (Env cs pad) <- ask
  let next = old <> delta dir
  if isConstrained next cs
    then do
      put next
      deref
    else do
      tell ["new position " <> show next <> " is out of bounds"]
      deref

line :: Array Dir -> PadRWS Char
line = map move >>> foldl (*>) (pure '-')

file :: Array (Array Dir) -> PadRWS (Array Char)
file = traverse line
