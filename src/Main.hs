module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath

import Data.Maybe
import Control.Monad

import qualified BF
import qualified PureBF

-- Options
data Action
  = ParseBF
  | ParseBF2
  | RunBF
  | RunBF2
  deriving (Show)

data Options = Options
  { optHelp :: Bool
  , optAction :: Action
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {optHelp = False, optAction = ParseBF}

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      "h?"
      ["help"]
      (NoArg (\opts -> opts {optHelp = True}))
      "Show help for the BrainFuck interpreter"
  , Option
      ""
      ["parseBF"]
      (NoArg (\opts -> opts {optAction = ParseBF}))
      "Parse using BF hand written parser"
  , Option
      ""
      ["runBF"]
      (NoArg (\opts -> opts {optAction = RunBF}))
      "Run (using BF hand written parser)"
  ]

usage :: String
usage = usageInfo header options
  where
    header = "Usage: bf [OPTION...] filename\n\n"

bfOptions :: [String] -> IO (Options, Maybe String)
bfOptions argv =
  case getOpt Permute options argv of
    (o, [n], []) -> return (foldr ($) defaultOptions o, Just n)
    (o, _, []) -> return (foldr ($) defaultOptions o, Nothing)
    (_, _, errs) -> ioError $ userError $ concat errs ++ usage

main :: IO ()
main = do
  argv <- getArgs
  (opts, fName) <- bfOptions argv
  print opts
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  when (isNothing fName) $ do
    putStrLn "You must supply exactly one source filename to bf\n"
    putStrLn usage
    exitFailure
  let fName' = fromJust fName
  source <- readFile fName'
  case optAction opts of
    RunBF -> print $ BF.run source ""
