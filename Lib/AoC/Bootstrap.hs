module AoC.Bootstrap
  (
    scaffold
  ) where

import Control.Monad (sequence)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO (readFile, putStrLn, getContents)
import System.Environment (getArgs)

noVersion = const $ pack "Unable to find a version to run"

readVersion :: [String] -> Maybe Int
readVersion [] = Nothing
readVersion args = case reads (head args) of
    [(i, _)] -> Just i
    _ -> Nothing

select :: [Text -> Text] -> Int -> Maybe (Text -> Text)
select impls v = case drop (v - 1) impls of
  (f:_) -> Just f
  _ -> Nothing

run :: [Text -> Text] -> [String] -> IO ()
run [] _ = putStrLn "No implementations provided yet"
run impls args = fmap impl TIO.getContents >>= TIO.putStrLn
  where
    impl = fromMaybe noVersion $ readVersion args >>= select impls


scaffold :: [Text -> Text] -> IO ()
scaffold fs = getArgs >>= run fs
