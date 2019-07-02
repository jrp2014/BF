module Main where

import           System.Console.GetOpt
import           System.Environment
import           System.Exit

--import System.FilePath
import           Control.Monad
import           Data.Maybe

import qualified BF
import qualified PureBF
import qualified BFKS
import qualified Text.Trifecta                 as P

-- Options
data Action
  = ParseBF
  | ParseBFKS
  | RunBF
  | RunPureBF
  | RunBFKS
  deriving (Show)

data Options = Options
  { optHelp :: Bool
  , optDebug :: Bool
  , optAction :: Action
  } deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options { optHelp = False, optDebug = False, optAction = ParseBF }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "d"
           ["debug"]
           (NoArg (\opts -> opts { optDebug = True }))
           "Show extra debugging output"
  , Option "h?"
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "Show help for the BrainFuck interpreter"
  , Option ""
           ["ParseBF"]
           (NoArg (\opts -> opts { optAction = ParseBF }))
           "Parse using BF hand written parser"
  , Option ""
           ["RunBFKS"]
           (NoArg (\opts -> opts { optAction = RunBFKS }))
           "Parse und run sing Trifecta BF parser"
  , Option ""
           ["ParseBFKS"]
           (NoArg (\opts -> opts { optAction = ParseBFKS }))
           "Parse using Trifecta BF parser"
  , Option ""
           ["RunPureBF"]
           (NoArg (\opts -> opts { optAction = RunPureBF }))
           "Parse using PureBFBF"
  , Option ""
           ["RunBF"]
           (NoArg (\opts -> opts { optAction = RunBF }))
           "Run (using BF hand written parser)"
  ]

usage :: String
usage = usageInfo header options
  where header = "Usage: bf [OPTION...] filename\n\n"

bfOptions :: [String] -> IO (Options, Maybe String)
bfOptions argv = case getOpt Permute options argv of
  (o, [n], []  ) -> return (foldr ($) defaultOptions o, Just n)
  (o, _  , []  ) -> return (foldr ($) defaultOptions o, Nothing)
  (_, _  , errs) -> ioError $ userError $ concat errs ++ usage

main :: IO ()
main = do
  argv          <- getArgs
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
    RunPureBF -> PureBF.interpret $ PureBF.compile source
    RunBF     -> if optDebug opts
      then print $ BF.run source ""
      else do
        let ((_, o), _, _) = BF.run source ""
        putStrLn o
    ParseBFKS -> print $ P.parseString BFKS.parseInstructions mempty source
    RunBFKS   -> BFKS.run source

