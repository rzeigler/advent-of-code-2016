module AoC.Day8 (
  light,
  rotateRow,
  rotateCol,
  commands,
  solvers
) where

import Prelude hiding (sum)

import Data.Foldable (foldl', sum)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator

import AoC.Combinator

data Command = Light Int Int
             | RotateRow Int Int
             | RotateCol Int Int
             deriving (Show, Eq)
             
type Screen = [Vector Bool]
             
eq :: Parser ()
eq = char '=' *> pure () 

rect :: Parser ()
rect = string "rect" *> pure()

row :: Parser ()
row = string "row" *> pure ()

col :: Parser ()
col = string "column" *> pure ()

rotate :: Parser ()
rotate = string "rotate" *> pure ()

int :: Parser Int
int = read <$> many1 digit

x :: Parser ()
x = char 'x' *> pure ()

y :: Parser ()
y = char 'y' *> pure ()

by :: Parser ()
by = string "by" *> pure ()

light :: Parser Command
light = rect *> spaces *> (Light <$> int <*> (x *> int))

rotateRow :: Parser Command
rotateRow = rotate *> space *> row *> space *> y *> eq *> (RotateRow <$> int <*> (space *> by *> space *> int))

rotateCol :: Parser Command
rotateCol = rotate *> space *> col *> space *> x *> eq *> (RotateCol <$> int <*> (space *> by *> space *> int))

commands :: Parser [Command]
commands = sepEndBy (light <|> try rotateRow <|> rotateCol) endOfLine

start :: Screen
start = replicate 6 (V.replicate 50 False)

asInt :: Bool -> Int
asInt True = 1
asInt False = 0

exec :: Screen -> Command -> Screen
exec s (Light x y) = undefined
exec s (RotateRow row n) = undefined
exec s (RotateCol col n) = undefined

solve_1 :: [Command] -> Int
solve_1 = foldl' exec start
  |> fmap (fmap asInt)
  |> fmap sum
  |> sum

solvers :: [Text -> Text]
solvers = [process commands solve_1]
