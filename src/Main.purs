module Main where

import Prelude
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE(), logShow, log)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (example, usage)

app :: forall eff. Maybe Int -> Maybe Int -> Eff (console :: CONSOLE, err :: EXCEPTION | eff) Unit
app (Just day) (Just sect) = do
  logShow day
  logShow sect
app _ _ = log "Section and Day must both be Integers"

main :: forall eff. Eff (console :: CONSOLE, err :: EXCEPTION | eff) Unit
main = do
  let setup = usage "$0 -d Day -s Section 'Input File'"
              <> example "$0 -d 1 -s 2 input.txt" "Run day 1 section 2 against input.txt"
  runY setup $ app <$> (fromNumber <$> yarg "d" ["day"] (Just "The day") (Right "A day is required") true)
                   <*> (fromNumber <$> yarg "s" ["section"] (Just "The section") (Left 1.0) false)
