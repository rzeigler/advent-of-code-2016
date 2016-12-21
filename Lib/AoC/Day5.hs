module AoC.Day5 (
  solvers
) where

import Prelude hiding (concat)
import Control.Monad.State
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

solve_1 :: Text -> Text
solve_1 = 
  md5s
  |> fmap (`index` 5)
  |> take 8
  |> pack  
  
solvers :: [Text -> Text]
solvers = [solve_1]
