module AoC.Day6 (
  solvers
) where

import Prelude hiding (lines)
import Data.List (foldl', foldl1', transpose)
import Data.Map.Strict (Map, empty, toList, adjust, member, insert)
import Data.Text (Text)
import qualified Data.Text as T

import AoC.Combinator

input :: Text -> [Text]
input = T.lines . T.strip

updateCounts :: Map Char Int -> Char -> Map Char Int
updateCounts m c
  | member c m = adjust (+1) c m
  | otherwise = insert c 1 m

iterateMostFrequent :: (Char, Int) -> (Char, Int) -> (Char, Int)
iterateMostFrequent (sc, si) (c, i)
  | si > i = (sc, si)
  | otherwise = (c, i)

mostCommonChar :: String -> Char
mostCommonChar =  fst . foldl1' iterateMostFrequent . toList . foldl' updateCounts empty

solve_1 :: Text -> Text
solve_1 = input
  |> fmap T.unpack
  |> transpose
  |> fmap mostCommonChar
  |> T.pack

solvers :: [Text -> Text]
solvers = [solve_1]
