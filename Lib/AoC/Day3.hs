module AoC.Day3 (
  solvers,
  int,
  tri,
  input,
  areTriSides,
  orderSides
) where

import Data.List (sortBy)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Prim hiding ((<|>), many)

import AoC.Combinator (process, (|>))

int :: Parser Int
int = read <$> many1 digit

mkTri :: Int -> Int -> Int -> [Int]
mkTri a b c = [a, b, c]

edge :: Parser Int
edge = spaces *> int

tri :: Parser [Int]
tri = (mkTri <$> edge <*> edge <*> edge) <* endOfLine

input :: Parser [[Int]]
input = many tri <* spaces

areTriSides :: [Int] -> Bool
areTriSides (a:b:c:_) = a < b + c
areTriSides _ = False -- because not a triangle...

orderSides :: [Int] -> [Int]
orderSides = sortBy (flip compare)

solve_1 :: [[Int]] -> Int
solve_1 =
  fmap orderSides
  |> filter areTriSides
  |> length

solvers :: [Text -> Text]
solvers = [process input solve_1]
