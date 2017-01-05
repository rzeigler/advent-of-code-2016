module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff, liftEff')
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Array (index, (!!))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (example, usage)
import Day1 as Day1

impls :: Array (Array (String -> String))
impls = [Day1.day]

impl :: Int -> Int -> Maybe (String -> String)
impl day sect = (impls !! (day - 1)) >>= (flip index) (sect - 1) 

badImpl :: forall eff. Eff (console :: CONSOLE | eff) Unit
badImpl = error "Invalid Day or Section"

runImpl :: forall eff. String -> (String -> String) -> Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | eff) Unit
runImpl file f = 
  let run = launchAff $ do
              output <- f <$> readTextFile UTF8 file 
              liftEff' (log output)
  in run *> pure unit
  

app :: forall eff. Maybe Int -> Maybe Int -> String -> Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | eff) Unit
app (Just day) (Just sect) file = maybe badImpl (runImpl file) (impl day sect)
app _ _ _ = log "Section and Day must both be Integers"

main :: forall eff. Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | eff) Unit
main = do
  let setup = usage "$0 -d Day -s Section -f Input"
              <> example "$0 -d 1 -s 2 -f input.txt" "Run day 1 section 2 against input.txt"
  runY setup $ app <$> (fromNumber <$> yarg "d" ["day"] (Just "The day") (Right "A day is required") true)
                   <*> (fromNumber <$> yarg "s" ["section"] (Just "The section") (Left 1.0) false)
                   <*> yarg "f" ["file"] (Just "the input file") (Right "an input file is required") true
