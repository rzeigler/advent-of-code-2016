module AoC.Day11 (
  
) where

import Control.Monad  
import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Combinator

data Component = Microchip String | Generator String
  deriving (Show, Eq)

instance Ord Component where
  compare (Microchip _) (Generator _) = LT
  compare (Generator _) (Microchip _) = GT
  compare (Microchip m1) (Microchip m2) = compare m1 m2
  compare (Generator g1) (Generator g2) = compare g1 g2  
  
data Configuration = Configuration Int (Vector [Component])
  deriving (Show, Eq)
  
canonicalize :: Configuration -> Configuration
canonicalize (Configuration e fs) = Configuration e (fmap L.sort fs)
  
shortinput = Configuration 0 (V.fromList [
                                [Microchip "Hydrogen", Microchip "Lithium"],
                                [Generator "Hydrogen"],
                                [Generator "Lithium"],
                                []
                              ])

promethium = "promethium"
cobalt = "cobalt"
curium = "curium"
ruthenium = "ruthenium"
plutonium = "plutonium"
  
                            
input = Configuration 0 (V.fromList [
                          [Generator promethium, Microchip promethium],
                          [Generator cobalt, Generator curium, Generator ruthenium, Generator plutonium],
                          [Microchip cobalt, Microchip curium, Microchip ruthenium, Microchip plutonium],
                          []
                        ])

isGoal :: Configuration -> Bool
isGoal (Configuration 3 fs) = all null (V.init fs)
isGoal _ = False

isSafeFrom :: Component -> Component -> Bool
isSafeFrom (Microchip _) _ = True
isSafeFrom (Generator _) (Generator _) = True
isSafeFrom (Generator g) (Microchip m) = g == m

isValidFloor :: [Component] -> Bool
isValidFloor cs = and (fmap (uncurry isSafeFrom) pairs)
  where pairs = [(c1, c2) | c1 <- cs, c2 <- cs]
  
isValid :: Configuration -> Bool
isValid (Configuration _ fs) = all isValidFloor fs

successors :: Configuration -> [Configuration]
successors = undefined
