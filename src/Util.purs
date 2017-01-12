module Util
  ( liftImpl
  , unwrap )
where

import Prelude
import Data.Maybe (Maybe(Just))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, runParser)

liftImpl :: forall a b. Show b => (Parser a) -> (a -> b) -> String -> String
liftImpl parser impl input = either show show (impl <$> runParser parser input)

unwrap :: forall a. Partial => Maybe a -> a
unwrap (Just a) = a
