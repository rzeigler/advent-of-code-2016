module Day1 (day) where

import Prelude
import Control.Monad.State as State
import Control.Alt ((<|>))
import Control.Monad.State (State, execState)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Int (floor)
import Data.List (List(..), elemIndex, null, reverse)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Global (readInt)
import Optic.Core (Lens', over, view, (..))
import Optic.Lens (lens)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)
import Text.Parsing.StringParser.String (anyDigit, char, eof, skipSpaces, whiteSpace)

data Dir = L | R

instance showDir :: Show Dir where
  show L = "L"
  show R = "R"

data Move = Move Dir Int

instance showMove :: Show Move where
  show (Move d i) = append (show d) (show i)

parseInt :: List Char -> Int
parseInt = foldl iter 0
  where
    iter :: Int -> Char -> Int
    iter s c = (s * 10) + floor (readInt 10 (fromCharArray [c]))

dirParser :: Parser Dir
dirParser = (char 'L' *> pure L)
            <|> (char 'R' *> pure R)

intParser :: Parser Int
intParser = parseInt <$> many1 anyDigit

moveParser :: Parser Move
moveParser = Move <$> dirParser <*> intParser

inputParser :: Parser (List Move)
inputParser = (sepBy1 moveParser (char ',' *> whiteSpace)) <* skipSpaces <* eof

data Compass = N | S | E | W

data Position = Position Compass (Tuple Int Int)

pointLens :: Lens' Position (Tuple Int Int)
pointLens = lens (\(Position _ p) -> p)
                 (\(Position c p) p' -> Position c p')

advance :: Position -> Position
advance (Position N (Tuple x y)) = Position N (Tuple x (y + 1))
advance (Position S (Tuple x y)) = Position S (Tuple x (y - 1))
advance (Position E (Tuple x y)) = Position E (Tuple (x + 1) y)
advance (Position W (Tuple x y)) = Position W (Tuple (x - 1) y)

rotate :: Dir -> Position -> Position
rotate L (Position N pt) = Position W pt
rotate L (Position W pt) = Position S pt
rotate L (Position S pt) = Position E pt
rotate L (Position E pt) = Position N pt
rotate R (Position N pt) = Position E pt
rotate R (Position E pt) = Position S pt
rotate R (Position S pt) = Position W pt
rotate R (Position W pt) = Position N pt

taxicab :: Tuple Int Int -> Int
taxicab (Tuple x y) = abs x + abs y


data Walk = Walk Position (List (Tuple Int Int))

positionLens :: Lens' Walk Position
positionLens = lens (\(Walk p _) -> p)
                    (\(Walk p c) p' -> Walk p' c)

coordsLens :: Lens' Walk (List (Tuple Int Int))
coordsLens = lens (\(Walk _ c) -> c)
                 (\(Walk p c) c' -> Walk p c')

rotator :: Dir -> State Walk Unit
rotator dir = State.modify (over positionLens (rotate dir))

advancer :: Int -> State Walk Unit
advancer 0 = State.modify id
advancer ct = do
  State.modify (over positionLens advance)
  point <- view (positionLens..pointLens) <$> State.get
  State.modify (over coordsLens (Cons point))
  advancer (ct - 1)

walk :: Move -> State Walk Unit
walk (Move dir ct) = do
  rotator dir
  advancer ct

start :: Position
start = Position N (Tuple 0 0)

simulate :: List Move -> Walk
simulate ls =
  let sim = foldl (*>) (advancer 0) (map walk ls)
  in execState sim (Walk start Nil)

part1Impl :: List Move -> Int
part1Impl = simulate
            >>> view (positionLens..pointLens)
            >>> taxicab

findDuplicate :: List (Tuple Int Int) -> Tuple Int Int
findDuplicate pts = unsafePartial $ run Nil pts
  where
    run :: Partial => List (Tuple Int Int) -> List (Tuple Int Int) -> Tuple Int Int
    run seen (Cons h rest) = case elemIndex h seen of
      Just _ -> h
      Nothing -> run (Cons h seen) rest

part2Impl :: List Move -> Int
part2Impl = simulate
            >>> view coordsLens
            >>> reverse
            >>> findDuplicate
            >>> taxicab

part1 :: String -> String
part1 input = either show show result
  where result = part1Impl <$> runParser inputParser input

part2 :: String -> String
part2 input = either show show result
  where result = part2Impl <$> runParser inputParser input

day :: Array (String -> String)
day = [part1, part2]
