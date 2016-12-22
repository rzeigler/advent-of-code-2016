module AoC.Day7 (
  solvers
) where

import Control.Applicative ((<|>))
import Data.List
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator

import AoC.Combinator

data Segment = AddrSeg String | HyperSeg String
type IPAddr = [Segment]

hyperParser :: Parser Segment
hyperParser = char '[' *> (HyperSeg <$> many1 lower) <* char ']'

addrParser :: Parser Segment
addrParser = AddrSeg <$> many1 lower

segmentParser :: Parser Segment
segmentParser = hyperParser <|> addrParser

ipParser :: Parser IPAddr
ipParser = many1 segmentParser

inputParser :: Parser [IPAddr]
inputParser = endBy ipParser endOfLine <* many endOfLine

isABBA :: Eq a => [a] -> Bool
isABBA (a1:b1:b2:a2:_) = a1 == a2 && b1 == b2 && a1 /= b1
isABBA _ = False

containsABBA :: String -> Bool
containsABBA s = isJust (find isABBA (aperture 4 s))

containsAddrABBA :: IPAddr -> Bool
containsAddrABBA = foldl' (||) False . fmap predicate
  where 
    predicate (AddrSeg str) = containsABBA str
    predicate (HyperSeg _) = False

containsHyperABBA :: IPAddr -> Bool
containsHyperABBA = foldl' (||) False . fmap predicate
  where 
    predicate (AddrSeg _) =  False
    predicate (HyperSeg str) = containsABBA str
    
solve_1 :: [IPAddr] -> Int
solve_1 = filter containsAddrABBA
  |> filter (not . containsHyperABBA)
  |> length
  
isTriple :: Eq a => [a] -> Bool
isTriple (a1:b:a2:_) = a1 == a2 && b /= a1
isTriple _ = False
  
findTriples :: Eq a => [a] -> [[a]]
findTriples = filter isTriple . aperture 3

-- Precondition inputs are already valid aba/bab triples
isCorresponding :: Eq a => ([a],[a]) -> Bool
isCorresponding (a1:b1:_, b2:a2:_) = a1 == a2 && b1 == b2
isCorresponding (_,_) = False
  
extractAddrTriples :: Segment -> [String]
extractAddrTriples (AddrSeg s) = findTriples s
extractAddrTriples _ = []
extractHyperTriples :: Segment -> [String]
extractHyperTriples (HyperSeg s) = findTriples s
extractHyperTriples _ = []
  
isSSL :: IPAddr -> Bool
isSSL addr = 
  let
    as = addr >>= extractAddrTriples
    hs = addr >>= extractHyperTriples
    pairs = [(a, h) | a <- as, h <- hs]
  in isJust (find isCorresponding pairs)
    
solve_2 :: [IPAddr] -> Int
solve_2 = filter isSSL
  |> length
  
solvers :: [Text -> Text]
solvers = [process inputParser solve_1, process inputParser solve_2]
