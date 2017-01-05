module Util
  (liftImpl)
where

import Prelude
import Data.Either (either)
import Text.Parsing.StringParser (Parser, runParser)

liftImpl :: forall a b. Show b => (Parser a) -> (a -> b) -> String -> String
liftImpl parser impl input = either show show (impl <$> runParser parser input)
