module AoC.Day1
  (
      Orient,
      Turn,
      Position,
      Motion,
      step,
      movement
  ) where

-- Implementation for https://adventofcode.com/2016/day/1  

import Text.Parsec 
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim hiding ((<|>), many)

newtype V2 = V2 (Int, Int)
  deriving (Show)
instance Monoid V2 where
  mappend (V2 (x1, y1)) (V2 (x2, y2)) = V2 (x1 + x2, y1 + y2)
  mempty = V2 (0, 0)


data Orient = N | S | E | W
  deriving (Show)
data Turn = R | L
  deriving (Show)

readTurn :: Char -> Turn
readTurn 'L' = L
readTurn 'R' = R

data Position = Position V2 Orient deriving (Show)
data Motion = Motion Turn Int deriving (Show)
  

-- Motion vector for a given direction
deltas = 
  [(N, V2 (0, 1)),
   (S, V2 (0, -1)),
   (E, V2 (1, 0)),
   (W, V2 (-1, 0))]

-- Rotation map
rotations =
  [(N, [(L, W), (R, E)]),
   (E, [(L, N), (R, S)]),
   (S, [(L, E), (R, W)]),
   (W, [(L, S), (R, N)])]

start = Position (V2 (0, 0)) N

-- Parsing input
step :: Parser Motion
step = Motion <$> turn <*> steps
  where 
    turn :: Parser Turn
    turn = readTurn <$> oneOf ['L', 'R']
    steps :: Parser Int
    steps = read <$> many1 digit
    digit = oneOf "1234567890"
    
movement :: Parser [Motion]
movement = sepBy1 step (string ", ")
