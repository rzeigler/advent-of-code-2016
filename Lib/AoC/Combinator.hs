module AoC.Combinator (
  (|>),
  unwrap,
  process,
  showT,
  aperture
) where

import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.Text

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

-- For use with the lookup tables
unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = undefined

render :: Show a => Either ParseError a -> Text
render = pack . either show show

process :: Show b => Parser a -> (a -> b) -> Text -> Text
process p f t = render $ fmap f (parse p "" t)

showT :: Show a => a -> Text
showT = pack . show

aperture :: Int -> [a] -> [[a]]
aperture n = run n []
  where 
    run :: Int -> [[a]] -> [a] -> [[a]]
    run n accum as =
      let 
        begin = take n as
      in 
        if length begin == n then run n (begin:accum) (tail as)
        else reverse accum
