module AoC.Combinator (
  (|>),
  unwrap,
  process,
  showT
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
