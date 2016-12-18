module AoC.Bootstrap 
  ( 
    scaffold
  ) where
     
import Control.Monad (sequence)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile, putStrLn)
import System.Environment (getArgs)
     
import Options.Applicative

data Args = Args Int String
    deriving (Show)
    
args :: Parser Args
args = Args 
    <$> option auto
        ( long "ver" 
          <> short 'v'
          <> metavar "N"
          <> help "Select which version to run 1, 2, etc." )
    <*> argument str (metavar "FILE")
    

run :: [(Text -> Text)] -> Args -> IO ()
run fs (Args v path) 
    | v > 0 && v <= length fs = fmap (fs !! (v - 1)) (TIO.readFile path) >>= TIO.putStrLn
    | otherwise = putStrLn $ "Available versions are in the range [1," ++ show (length fs + 1) ++ ")"
-- Scaffold a main function
scaffold :: [(Text -> Text)] -> IO ()
scaffold fs = execParser opts >>= run fs
  where
      opts = info (helper <*> args)
        ( fullDesc
       <> progDesc "Print a greeting for TARGET"
       <> header "hello - a test for optparse-applicative" )
    