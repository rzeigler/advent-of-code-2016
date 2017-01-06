module Util
  ( liftImpl
  , unsafeCoerceMaybe
  )
where

import Prelude
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, runParser)

liftImpl :: forall a b. Show b => (Parser a) -> (a -> b) -> String -> String
liftImpl parser impl input = either show show (impl <$> runParser parser input)

unsafeCoerceMaybe :: forall a. Maybe a -> a
unsafeCoerceMaybe m = unsafePartial $ case m of
  Just a -> a
