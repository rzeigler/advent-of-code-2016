{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Implementation for https://adventofcode.com/2016/day/1
module AoC.Day1
  (
      Orient(..),
      Turn(..),
      Position(..),
      Motion(..),
      V2(..),
      start,
      step,
      movement,
      advance,
      rotate,
      update,
      updates,
      solve_1,
      solvers
  ) where

import Control.Monad
import Data.Text (Text, pack)
import Data.List
import Data.Function ((&))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Prim hiding ((<|>), many)

import AoC.Combinator ((|>), unwrap)

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

coord :: Position -> V2
coord (Position v2 _) = v2

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

lookupRotation :: Turn -> Orient -> Orient
lookupRotation t o = unwrap $ lookup o rotations >>= lookup t

rotate :: Turn -> Position -> Position
rotate turn (Position coord orient) = Position coord (lookupRotation turn orient)

advance :: Int -> Position -> [Position]
advance n (Position coord orient) = fmap (flip Position orient) coords
  where
    coords = scanl' mappend coord updates
    updates = replicate n delta
    delta = unwrap (lookup orient deltas)

update :: Motion -> Position -> Position
update m p = last (updates m p)

updates :: Motion -> Position -> [Position]
updates (Motion t n) p = tail $ advance n $ rotate t p

taxicab :: V2 -> Int
taxicab (V2 (x, y)) = abs x + abs y

parseInput :: Text -> Either ParseError [Motion]
parseInput = parse movement ""

walk :: [Motion] -> Int
walk =
  fmap update
  |> foldl' (&) start
  |> coord
  |> taxicab

solve_1 :: Text -> Text
solve_1 t =
  let result = fmap walk (parseInput t)
  in
    pack $ either show show result

firstDuplicate :: Eq a => [a] -> a
firstDuplicate = go []
  where
    go seen (a:rest)
      | a `elem` seen = a
      | otherwise = go (a:seen) rest

walk2 :: [Motion] -> Int
walk2 =
  fmap updates
  |> scanl' ((&) . last) [start]
  |> join
  |> fmap coord
  |> firstDuplicate
  |> taxicab

solve_2 :: Text -> Text
solve_2 t =
  let result = fmap walk2 (parseInput t)
  in
    pack $ either show show result

solvers = [solve_1, solve_2]
