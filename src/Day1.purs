module Day1 (day) where

import Prelude
import Data.List (List, toUnfoldable)
import Data.String (fromCharArray)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (anyDigit, char, many1)

data Dir = L | R
data Move = Move Dir Int

dirParser :: Parser Dir
dirParser = (char 'L' *> pure L)
            <|> (char 'R' *> pure R)
            
intParser :: Parser Int
intParser = fromCharArray . toUnfoldable <$> many1 anyDigit

moveParser :: Parser Move
moveParser = Move <$> dirParser <*> intParser

part_1 :: String -> String
part_1 = id

day :: Array (String -> String)
day = [part_1]
