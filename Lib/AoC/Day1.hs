{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Implementation for https://adventofcode.com/2016/day/1
module AoC.Day1
  (
      Orient(N, S, E, W),
      Turn,
      Position(),
      Motion,
      V2,
      start,
      step,
      movement,
      advance,
      rotate,
      update,
      solve_1,
      solvers
  ) where

import Data.Text (Text, pack)
import Data.List
import Data.Function ((&))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Prim hiding ((<|>), many)

data V2 = V2 (Int, Int)
  deriving (Show, Eq)
instance Monoid V2 where
  mappend (V2 (x1, y1)) (V2 (x2, y2)) = V2 (x1 + x2, y1 + y2)
  mempty = V2 (0, 0)

data Orient = N | S | E | W
  deriving (Show, Eq)
data Turn = R | L
  deriving (Show, Eq)

readTurn :: Char -> Turn
readTurn 'L' = L
readTurn 'R' = R

data Position = Position V2 Orient deriving (Show, Eq)
data Motion = Motion Turn Int deriving (Show, Eq)


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

-- For use with the lookup tables
unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = undefined

lookupRotation :: Turn -> Orient -> Orient
lookupRotation t o = unwrap $ lookup o rotations >>= lookup t

rotate :: Turn -> Position -> Position
rotate turn (Position coord orient) = Position coord (lookupRotation turn orient)

advance :: Int -> Position -> Position
advance n (Position coord orient) = Position coord' orient
  where
    coord' = foldr mappend coord updates
    updates = replicate n delta
    delta = unwrap (lookup orient deltas)

update :: Motion -> Position -> Position
update (Motion t n) p = advance n $ rotate t p

taxicab :: Position -> Int
taxicab (Position (V2 (x, y)) _) = abs x + abs y

solve_1 :: Text -> Text
solve_1 t =
  let
    moves = parse movement "" t
    updates = fmap (fmap update) moves
    result = fmap (taxicab . foldl (&) start) updates
  in
    pack $ either show show result

solvers = [solve_1]
