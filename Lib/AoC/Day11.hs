module AoC.Day11 (
  search,
  shortinput,
  fullinput,
  fullinput2
) where

-- import Debug.Trace
import Control.Monad  
import qualified Data.List as L
import Data.Vector (Vector, (!), concat)
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as H
import qualified Data.Vector as V
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Combinator
  
data Floors = Floors Int (Vector String) (Vector Int) Int
  deriving (Show, Eq)

instance Hashable Floors where
  hashWithSalt i (Floors _ _ fs e) = i * sum (V.zipWith power fs (V.fromList [0..(length fs)]))

power :: Int -> Int -> Int
power d p = d * 10 ^ p

targetFloor = 3
  
shortinput = Floors 2 (V.fromList ["hydrogen", "lithium"]) (V.fromList [0, 1, 0, 2]) 0

fullinput = Floors 5 
               (V.fromList ["promethium", "cobalt", "curium", "ruthenium", "plutonium"]) 
               (V.fromList [ 0, 0, 2, 1, 2, 1, 2, 1, 2, 1 ])
               0                 
               
fullinput2 = Floors 7
             (V.fromList ["promethium", "cobalt", "curium", "ruthenium", "plutonium", "elerium", "dilithium"]) 
             (V.fromList [ 0, 0, 2, 1, 2, 1, 2, 1, 2, 1 , 1, 1, 1, 1])
             0                 


-- For the nth component type produce the vector index containing the floor of the microchip
offsetToChipIdx :: Int -> Int
offsetToChipIdx n = n * 2
-- For the nth component type produce the vector index containing the floor of the generator
offsetToGeneratorIdx :: Int -> Int
offsetToGeneratorIdx = (+1) . offsetToChipIdx

isChipSafe :: Floors -> Int -> Bool
isChipSafe (Floors len _ fs e) i = fs ! chip == fs ! generator || notElem (fs ! chip) otherGeneratorFloors
  where 
    otherGeneratorFloors = fmap (fs !) otherGenerators
    chip = offsetToChipIdx i
    generator = offsetToGeneratorIdx i
    otherGenerators = fmap offsetToGeneratorIdx (L.delete i [0..(len - 1)])
    
isValid :: Floors -> Bool
isValid conf = all (isChipSafe conf) [0..(len - 1)]
  where 
    (Floors len _ _ _) = conf
    
isGoal :: Floors -> Bool
isGoal (Floors _ _ fs e) = e == targetFloor && V.all (targetFloor==) fs

add :: Vector Int -> Vector Int -> Vector Int
add = V.zipWith (+)  

down :: Int
down = -1
up :: Int
up = 1

data Direction = Up | Down
data Move = Single Int | Pair Int Int deriving (Eq, Show)
data Update = Update (Vector Int) Int  deriving (Eq, Show)

diff :: Direction -> Int
diff Up = up
diff Down = down

delta :: Int -> Direction -> Move -> Update
delta n d (Pair i1 i2) = 
    Update (V.update (V.replicate n 0) (V.fromList [(i1, dir), (i2, dir)])) dir 
  where dir = diff d
delta n d (Single i) = 
    Update (V.update (V.replicate n 0) (V.fromList [(i, dir)])) dir 
  where dir = diff d
  
apply :: Floors -> Update  -> Floors
apply (Floors len names fs e) (Update v i) = Floors len names (add v fs) (i + e)

successorsTo :: Floors -> [Floors]
successorsTo current = filter isValid (apply current <$> (posUpdates ++ negUpdates))
  where
      (Floors len _ fs e) = current
      upper = delta (len * 2) Up
      downer = delta (len * 2) Down
      moveable = filter onTargetFloor [0..(len * 2) - 1]
      moveablePairs = [(x, y) | x <- moveable, y <- moveable, x < y]
      posUpdates = if e < 3 then (upper . Single <$> moveable) ++ (upper . uncurry Pair <$> moveablePairs)  else []
      negUpdates = if e > 0 then (downer . Single <$> moveable) ++ (downer . uncurry Pair <$> moveablePairs)  else []
      onTargetFloor i = fs ! i == e
      
search :: Floors -> (Int, Floors)
search = run H.empty 0 . H.singleton
  where 
    run :: HashSet Floors -> Int -> HashSet Floors -> (Int, Floors)
    run seen dist next = 
      let
        fringe = H.difference next seen
        found = H.filter isGoal fringe
      in
        if null found 
          then run (H.union fringe seen) (dist + 1)  (H.fromList (H.toList fringe >>= successorsTo)  )
          else (dist, head (H.toList found))
        