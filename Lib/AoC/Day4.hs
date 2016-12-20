module AoC.Day4 (
  Room,
  hash,
  sector,
  checksum,
  roomParser,
  hashLetterCompare,
  isRealRoom,
  order,
  solvers
) where

import GHC.Exts (groupWith)
import Data.Char
import Data.List (sortBy, intercalate, elemIndex, isInfixOf)
import Data.Text (Text)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Prim hiding ((<|>), many)

import AoC.Combinator

data Room = Room { hash :: String, sector :: Int, checksum :: String }
  deriving (Eq, Show)

hashParser :: Parser String
hashParser = concat <$> endBy1 (many1 letter) (char '-')

sectorIdParser :: Parser Int
sectorIdParser = read <$> many1 digit

checksumParser :: Parser String
checksumParser = char '[' *> many1 letter <* char ']'

roomParser :: Parser Room
roomParser = Room <$> hashParser <*> sectorIdParser <*> checksumParser

inputParser :: Parser [Room]
inputParser = endBy roomParser endOfLine

hashLetterCompare :: Ord a => [a] -> [a] -> Ordering
hashLetterCompare l1s l2s =
  case compare (length l2s) (length l1s) of
    LT -> LT
    GT -> GT
    EQ -> compare (head l1s) (head l2s)

order :: String -> String
order = fmap head . sortBy hashLetterCompare . groupWith id

isRealRoom :: Room -> Bool
isRealRoom r = compute r == checksum r
  where compute = take 5 . order . hash

solver_1 :: [Room] -> Int
solver_1 =
  filter isRealRoom
  |> fmap sector
  |> sum

alphabet = "abcdefghijklmnopqrstuvwxyz"

rotate :: Int -> Char -> Char
rotate n c = alphabet !! mod (base + n) 26
  where
    base = unwrap (elemIndex c alphabet)


decode :: Room -> Room
decode (Room h s c) = Room (fmap (rotate s) h) s c

solver_2 :: [Room] -> [Room]
solver_2 =
  filter isRealRoom
  |> fmap decode
  |> filter (isInfixOf "northpole" . hash)

solvers :: [Text -> Text]
solvers = [process inputParser solver_1, process inputParser solver_2]
