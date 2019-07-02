module Main where

-- https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md

import           Data.Char
import           System.IO
import System.Environment

data BrainfuckCommand
  = GoRight -- >
  | GoLeft -- <
  | Increment -- +
  | Decrement -- -
  | Print -- .
  | Read -- ,
  | LoopL -- [
  | LoopR -- ]
  | Comment Char -- anything else
  deriving (Show)


type BrainfuckSource = [BrainfuckCommand]

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = map charToBF
 where
  charToBF x = case x of
    '>' -> GoRight
    '<' -> GoLeft
    '+' -> Increment
    '-' -> Decrement
    '.' -> Print
    ',' -> Read
    '[' -> LoopL
    ']' -> LoopR
    c   -> Comment c

data Tape a =
  Tape [a] -- Left of the pivot element
       a -- Pivot element
       [a] -- Right of the pivot element

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
 where
  zeros :: [Int]
  zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r : rs)) = Tape (p : ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l : ls) p rs) = Tape ls l (p : rs)

advance
  :: Tape Int -- Data tape
  -> Tape BrainfuckCommand -- Instruction tape
  -> IO ()
advance dataTape (Tape _ _ []) = return ()
advance dataTape source        = run dataTape (moveRight source)

-- Interpret the command currently focussed on the instruction tape
run
  :: Tape Int -- Data tape
  -> Tape BrainfuckCommand -- Instruction tape
  -> IO ()
run dataTape source@(Tape _ GoRight _) = advance (moveRight dataTape) source
run dataTape source@(Tape _ GoLeft  _) = advance (moveLeft dataTape) source
run (Tape l p r) source@(Tape _ Increment _) =
  advance (Tape l (p + 1) r) source
run (Tape l p r) source@(Tape _ Decrement _) =
  advance (Tape l (p - 1) r) source
run dataTape@(Tape _ p _) source@(Tape _ Print _) = do
  putChar (chr p)
  hFlush stdout
  advance dataTape source
run dataTape@(Tape l _ r) source@(Tape _ Read _) = do
  p <- getChar
  advance (Tape l (ord p) r) source
run dataTape@(Tape _ p _) source@(Tape _ LoopL _)
  |
    -- If the pivot is zero, jump to the
    -- corresponding LoopR instruction
    p == 0    = seekLoopR 0 dataTape source
  |
    -- Otherwise just ignore the `[` and continue
    otherwise = advance dataTape source
run dataTape@(Tape _ p _) source@(Tape _ LoopR _)
  | p /= 0    = seekLoopL 0 dataTape source
  | otherwise = advance dataTape source
run dataTape source@(Tape _ (Comment _) _) = advance dataTape source


-- Move the instruction pointer left until a "[" is found.
-- The first parameter ("b" for balance) retains the current
-- bracket balance to find the matching partner. When b is 1,
-- then the found LoopR would reduce the counter to zero,
-- hence we break even and the search is successful.
seekLoopR
  :: Int                   -- Parenthesis balance
  -> Tape Int              -- Data tape
  -> Tape BrainfuckCommand -- Instruction tape
  -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) =
  seekLoopR (b - 1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) =
  seekLoopR (b + 1) dataTape (moveRight source)
seekLoopR b dataTape source = seekLoopR b dataTape (moveRight source)

seekLoopL
  :: Int                   -- Parenthesis balance
  -> Tape Int              -- Data tape
  -> Tape BrainfuckCommand -- Instruction tape
  -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) =
  seekLoopL (b - 1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) =
  seekLoopL (b + 1) dataTape (moveLeft source)
seekLoopL b dataTape source = seekLoopL b dataTape (moveLeft source)




runBrainfuck :: BrainfuckSource -> IO ()
runBrainfuck = run emptyTape . bfSource2Tape
 where
  bfSource2Tape :: BrainfuckSource -> Tape BrainfuckCommand
  bfSource2Tape []       = Tape [] (Comment '!') []
  bfSource2Tape (b : bs) = Tape [] b bs


main :: IO ()
main = do
  fn <- getArgs
  prog <- readFile (head fn)
  runBrainfuck  (parseBrainfuck prog)
