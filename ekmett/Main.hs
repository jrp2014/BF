{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-
 See https://very.science/pdf/GQFC.pdf
 and hackage package Data.Streams.Tape (which needs an updated cabal file)
 -}

module Main where

import Control.Comonad
  ( Comonad (duplicate, extract),
    ComonadApply ((<@>)),
    kfix,
  )
import Control.Monad.Fix (fix)
import Control.Monad.State
  ( MonadTrans (lift),
    StateT,
    execStateT,
    gets,
    modify,
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)
import Data.Stream (Stream (..))
import qualified Data.Stream as Stream
import System.Environment (getArgs)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdin,
    stdout,
  )
import Tape
import Prelude hiding (Either(..))


-- From E Kmett's
-- https://github.com/ekmett/lens/blob/bec00420db73cacb2bb8a277ca115d2220ef2c76/examples/BrainfuckFinal.hs
-- but using Tape, rather than a ZipList and doing real IO
----------------------------------

-- Low level syntax form

data Instr = Plus | Minus | Right | Left | Comma | Dot | Open | Close deriving Show

type Code = [Instr]

parse :: String -> Code
parse = mapMaybe (`lookup` symbols)
  where
    symbols =
      [ ('+', Plus),
        ('-', Minus),
        ('<', Left),
        ('>', Right),
        (',', Comma),
        ('.', Dot),
        ('[', Open),
        (']', Close)
      ]

-- Higher level semantic graph

-- * State/IO-based interpreter

type MachineState = Tape Int

type Program = StateT MachineState IO ()

compile :: Code -> Program
compile = fst . bracket []

branch :: Program -> Program -> Program
branch z n = do
  c <- gets focus
  if c == 0 then z else n

-- | Takes a continuation and a list of instructions and produces
-- forward and a stack of backward continuations
--  [forward continuations] -> code  -> (next program, backward continuations)
bracket :: [Program] -> Code -> (Program, [Program])
bracket [] [] = (return (), [])
bracket _ [] = error "Mismatched opening bracket"
bracket [] (Close : _) = error "Mismatched closing bracket"
-- Match a closing bracket: Pop a forward continuation, push backwards
bracket (c : cs) (Close : xs) = (branch n c, n : bs)
  where
    (n, bs) = bracket cs xs

-- Match an opening bracket: Pop a backwards continuation, push forwards
bracket cs (Open : xs) = (branch b n, bs)
  where
    (n, b : bs) = bracket (n : cs) xs -- note recursion

-- Match any other symbol in the trivial way
bracket cs (x : xs) = first (run x >>) (bracket cs xs)

run :: Instr -> Program
run Plus = modify (\tape -> tape {focus = succ (focus tape)})
run Minus =  modify (\tape -> tape {focus = pred (focus tape)})
run Right = modify moveR
run Left = modify moveL
run Comma = do
  c <- lift getChar
  modify (\tape -> tape {focus = ord c})
run Dot = do
  c <- gets focus
  lift $ putChar (chr c)
run _ = error "undefined instruction"

initial :: MachineState
initial = Tape (Stream.repeat 0) 0 (Stream.repeat 0)

interpret :: Program -> IO MachineState
interpret = flip execStateT initial

eval :: String -> IO MachineState
eval = interpret . compile . parse

main :: IO ()
main = do
  as <- getArgs
  case as of
    -- STDIN is program
    [] -> do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      _tape <- getContents >>= eval
      return ()

    -- STDIN is input
    [f] -> do
      _tape <- readFile f >>= eval
      return ()

    -- Malformed command line
    _ -> putStrLn "Usage: brainfuck [program]"
