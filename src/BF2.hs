module BF2 where

import Control.Applicative
import Data.Char
import Data.Monoid

import NanoParsec

type Source = String

type I = Int

type Cell = Sum I

type Tape = ([Cell], [Cell])

type Step = Tape -> Tape

type Steps = [Step]

type Input = Char

type Output = Char

type World = ([Input], [Output])

data Instruction
  = Increment
  | Decrement
  | Back
  | Forward
  | Accept
  | Emit
  | Loop Program
  deriving (Show)

type Program = [Instruction]

type Environment = (World, Program, Tape)

prog :: Parser Program
prog = do
  _ <- spaces'
  some instruction

instruction :: Parser Instruction
instruction = simpleInstruction <|> loopInstruction

simpleInstruction :: Parser Instruction
simpleInstruction =
  anInstruction "+" Increment <|> anInstruction "-" Decrement <|>
  anInstruction ">" Forward <|>
  anInstruction "<" Back <|>
  anInstruction "," Accept <|>
  anInstruction "." Emit

anInstruction :: String -> Instruction -> Parser Instruction
anInstruction s i = reserved' s >> return i

loopInstruction :: Parser Instruction
loopInstruction = do
  p <- brackets prog
  return $ Loop p

token' :: Parser a -> Parser a
token' p = do
  a <- p
  _ <- spaces'
  return a

reserved' :: String -> Parser String
reserved' s = token' (string s)

spaces' :: Parser String
spaces' = many $ noneOf "+-><,.[]"

increment :: Step
increment (l, x:xs) = (l, x + 1 : xs)
increment (l, []) = (l, [1])

decrement :: Step
decrement (l, x:xs) = (l, x - 1 : xs)
decrement (l, []) = (l, [-1])

back :: Step
back (x:xs, r) = (xs, x : r)
back ([], r) = ([], 0 : r)

forward :: Step
forward (l, x:xs) = (x : l, xs)
forward (l, []) = (0 : l, [])

accept :: Input -> Step
accept c (l, _:xs) = (l, (fromIntegral . ord) c : xs)
accept c (l, []) = (l, [(fromIntegral . ord) c])

emit :: Tape -> Output
emit (_, Sum x:_) = chr x
emit (_, []) = chr 0

loop :: Tape -> Bool
loop (_, 0:_) = False
loop (_, []) = False
loop (_, _) = True

exec :: Environment -> Environment
exec (w, [], t) = (w, [], t)
exec (w, Increment:xs, t) = exec (w, xs, increment t)
exec (w, Decrement:xs, t) = exec (w, xs, decrement t)
exec (w, Back:xs, t) = exec (w, xs, back t)
exec (w, Forward:xs, t) = exec (w, xs, forward t)
exec ((a:as, e), Accept:xs, t) = exec ((as, e), xs, accept a t)
--exec (([], o), p@(Accept:_), t) = (([],o), p, t)
exec (([], _), Accept:_, _) = error "Premature end of input"
exec ((a, e), Emit:xs, t) = exec ((a, e ++ [emit t]), xs, t)
exec (w, ll@(Loop l:xs), t) =
  if loop t
    then let (w', _, t') = exec (w, l, t) -- Execute the loop
          in exec (w', ll, t') -- and try again
    else exec (w, xs, t) -- Skip the loop

run :: Source -> [Input] -> Environment
run s i = exec ((i ++ [chr 0], []), runParser prog s, ([], []))

add :: Source
add = "++>+<-"

hw :: Source
hw =
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

fib :: Source
fib =
  "+++++++++++" ++
  ">+>>>>++++++++++++++++++++++++++++++++++++++++++++" ++
  ">++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>" ++
  "+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-" ++
  "<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<" ++
  "-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]" ++
  ">[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++" ++
  "+++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++" ++
  "++++++++++++++++++++++++++++++++++++++++++++.[-]<<" ++
  "<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<" ++
  "[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"

quine :: Source
quine =
  ">>>>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>+>+>+>+>+>+>+>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++++>+>+>+>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++++>+>+>++++++++++++++++++++>+>+>+>++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>+>++++++++++++++++++++>+++>++++>++++>++++>++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++>++++++++++++++++++>++++>++++++++++++++++++>+>++++++++++++++++++++>++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++++>+>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++>+>++++++++++++++++++++>++++++++++++++++++++>+>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++><[<]<<<+++++++[->+++<]>[->++>+++<<]>+>-....>[[-<<.<+>>>]<.[->+<]<[-<+>>+<]>>>]<<<<[<]>[-.>]"

quineTest :: Bool
quineTest = out == quine
  where
    ((_, out), _, _) = run quine ""
