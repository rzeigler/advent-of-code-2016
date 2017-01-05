module Day1 (day) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Int (floor)
import Data.List (List)
import Data.Ord (abs)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Global (readInt)
import Optic.Core (Lens', over, (..))
import Optic.Lens (lens)
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

part1Impl :: List Move -> Int
part1Impl ms = 0

part1 :: String -> String
part1 input = either show show result
  where result = part1Impl <$> runParser inputParser input

day :: Array (String -> String)
day = [part1]
