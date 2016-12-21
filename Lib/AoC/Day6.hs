module AoC.Day6 (
  solvers
) where

import Prelude hiding (lines)
import Data.Ord
import Data.List (foldl', foldl1', transpose, maximumBy, minimumBy)
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


histogram :: String -> [(Char, Int)]
histogram = toList . foldl' updateCounts empty

mostCommon :: [(Char, Int)] -> Char
mostCommon = fst . maximumBy (comparing snd)

leastCommon :: [(Char, Int)] -> Char
leastCommon = fst . minimumBy (comparing snd)

histograms :: Text -> [[(Char, Int)]]
histograms = input
  |> fmap T.unpack
  |> transpose
  |> fmap histogram

solve_1 :: Text -> Text
solve_1 = histograms
  |> fmap mostCommon
  |> T.pack

solve_2 :: Text -> Text
solve_2 = histograms
  |> fmap leastCommon
  |> T.pack

solvers :: [Text -> Text]
solvers = [solve_1, solve_2]
