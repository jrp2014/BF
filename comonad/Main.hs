{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Prelude hiding (Either (..), iterate)

-- | A @Tape@ is like a Turing-machine tape: infinite in both directions, with a focus in the middle.
data Tape a = Tape
  { -- | the side of the @Tape@ left of @focus@
    viewL :: Stream a,
    -- | the focused element
    focus :: a,
    -- | the side of the @Tape@ right of @focus@
    viewR :: Stream a
  }
  deriving (Functor)

-- | The functions @moveR@ and @moveL@ move the focus on the tape right and left, respectively.
moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)
moveR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs

-- | Gives a @Tape@ containing infinite copies of the given element.
tapeOf :: a -> Tape a
tapeOf = pure

-- | Produce a @Tape@ from a seed value, ala unfoldr for lists, or unfold for @Stream@s.
unfold ::
  -- | leftwards unfolding function
  (c -> (a, c)) ->
  -- | function giving the focus value from the seed
  (c -> a) ->
  -- | rightwards unfolding function
  (c -> (a, c)) ->
  -- | seed value
  c ->
  Tape a
unfold prev center next =
  Tape <$> Stream.unfold prev <*> center <*> Stream.unfold next

-- | Produce a @Tape@ consisting of the infinite iteration of two functions to a starting focus value,
--   ala iterate for lists or @Stream@s.
iterate ::
  -- | leftwards iteration function
  (a -> a) ->
  -- | rightwards iteration function
  (a -> a) ->
  -- | focus value
  a ->
  Tape a
--iterate prev next seed = Tape (Stream.iterate prev (prev seed)) seed (Stream.iterate next (next seed))
iterate prev next =
  unfold (dup . prev) id (dup . next)
  where
    dup a = (a, a)

instance Comonad Stream where
  extract = Stream.head
  duplicate = Stream.tails

instance ComonadApply Stream where (<@>) = (<*>)
--  Cons x xs <@> Cons x' xs' = Cons (x x') (xs <*> xs')

-- | Tapes form a comonad, where extract gives the focus element and duplicate gives a /diagonalized/
--   @Tape (Tape a)@ such that @extract . extract . moveL . duplicate == extract . moveL@ and likewise
--   for @moveR@.
instance Comonad Tape where
  extract = focus
  duplicate = iterate moveL moveR

-- | Applying one tape to another moves them together. This is like the @Applicative@ instances for
--   @ZipList@ and @Stream@.
instance ComonadApply Tape where
  (Tape ls c rs) <@> (Tape ls' c' rs') = Tape (ls <@> ls') (c c') (rs <@> rs')

-- | A tape is @Applicative@, where the @\<*\>@ is equivalent to its @ComonadApply@ instance (required
--   by law), and a pure value is the tape consisting of copies of that value in both directions.
instance Applicative Tape where
  (<*>) = (<@>)
  pure = Tape <$> pure <*> id <*> pure

-- this is really evaluate / kfix
lfix :: ComonadApply w => w (w a -> a) -> w a
lfix f = fix ((f <@>) . duplicate)

tenKilo :: Tape (Tape a -> a) -> [a]
tenKilo = Stream.take 10000 . viewR . kfix

t :: Tape (Tape Int -> Int)
t =
  Tape
    (Stream.repeat (const 0)) -- zero left of origin
    (const 0) -- zero at origin
    ( Stream.repeat -- right of origin:
        (succ . extract . moveL) -- 1 + leftward value
    )

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
