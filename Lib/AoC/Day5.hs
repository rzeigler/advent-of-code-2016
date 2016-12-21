module AoC.Day5 (
  solvers
) where

import Prelude hiding (concat)
import Control.Monad.State.Lazy
import GHC.Exts (sortWith)
import Data.Char (digitToInt)
import Data.Text (Text, append, pack, intercalate, isPrefixOf, index, concat, stripEnd)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

import Crypto.Hash

import AoC.Combinator

unroll :: Text -> [Text]
unroll s = fmap (append s . showT) [0..]

md5ify :: Text -> Text
md5ify = showT . (hash :: ByteString -> Digest MD5) . encodeUtf8

md5s :: Text -> [Text]
md5s =
  stripEnd
  |> unroll
  |> fmap md5ify
  |> filter (isPrefixOf (pack "00000"))

hashLetter :: Text -> Char
hashLetter = (`index` 5)

solve_1 :: Text -> Text
solve_1 =
  md5s
  |> fmap hashLetter
  |> take 8
  |> pack

type IdxChar = (Int, Char)

md5ToIndexedChar :: Text -> IdxChar
md5ToIndexedChar t = (digitToInt (index t 5), index t 6)

isValidIndexedChar :: (Int, Char) -> Bool
isValidIndexedChar (i, _) = i >= 0 && i < 8

-- Input index is assumed valid
addIndexedChar :: IdxChar -> [IdxChar] -> [IdxChar]
addIndexedChar (i, c) s
  | i `elem` fmap fst s = s
  | otherwise = (i, c):s

-- Assumes that only valid codes are in the list
isCompleteCode :: [IdxChar] -> Bool
isCompleteCode = (8==) . length

empty :: [IdxChar]
empty = []

render :: [IdxChar] -> Text
render = pack . fmap snd . sortWith fst

solve_2 :: Text -> Text
solve_2 = md5s
  |> fmap md5ToIndexedChar
  |> filter isValidIndexedChar
  |> scanl (flip addIndexedChar) empty
  |> dropWhile (not . isCompleteCode)
  |> head
  |> render

solvers :: [Text -> Text]
solvers = [solve_1, solve_2]
