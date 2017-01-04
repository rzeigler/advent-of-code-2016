module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (example, usage)

impls :: Array (Array String -> String)
impls = []

app :: forall eff. Maybe Int -> Maybe Int -> String -> Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | eff) Unit
app (Just day) (Just sect) file = run *> pure unit
  where
    run = launchAff $ do
      text <- readTextFile UTF8 file
      liftEff' (log text)

app _ _ _ = log "Section and Day must both be Integers"

main :: forall eff. Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | eff) Unit
main = do
  let setup = usage "$0 -d Day -s Section -f Input"
              <> example "$0 -d 1 -s 2 -f input.txt" "Run day 1 section 2 against input.txt"
  runY setup $ app <$> (fromNumber <$> yarg "d" ["day"] (Just "The day") (Right "A day is required") true)
                   <*> (fromNumber <$> yarg "s" ["section"] (Just "The section") (Left 1.0) false)
                   <*> yarg "f" ["file"] (Just "the input file") (Right "an input file is required") true
