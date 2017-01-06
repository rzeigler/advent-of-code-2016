module Day2 where

import Prelude
import Data.Generic (class Generic, gShow)

data Direction = U | D | L | R

derive instance genericDirection :: Generic Direction

instance showDirection :: Show Direction where
  show = gShow
