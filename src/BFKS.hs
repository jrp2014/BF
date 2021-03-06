module BFKS where


-- From Daniel Silverstone
-- https://www.youtube.com/watch?v=VvajXPyKuTo&t=119s

import qualified Text.Trifecta                 as P
import           Control.Monad.State
import qualified Data.IntMap                   as M
import           Data.Word
import           Data.Maybe                     ( fromMaybe )

type Source = String

data Instruction
  = Increment
  | Decrement
  | Back
  | Forward
  | Accept
  | Emit
  | Loop [Instruction]
  deriving (Show)


parseGen :: Char -> Instruction -> P.Parser Instruction
parseGen c i = P.char c >> return i

parseIncrement, parseDecrement, parseBack, parseForward, parseAccept, parseEmit, parseLoop, parseInstruction
  :: P.Parser Instruction
parseBack = parseGen '<' Back
parseForward = parseGen '>' Forward
parseIncrement = parseGen '+' Increment
parseDecrement = parseGen '-' Decrement
parseAccept = parseGen ',' Accept
parseEmit = parseGen '.' Emit
parseLoop = Loop <$> P.brackets parseInstructions

parseComment :: P.Parser ()
parseComment =  --  P.skipOptional $ P.noneOf "<>+-,.[" -- \n terminates parse
               do
  _ <- P.many $ P.noneOf "+-<>.+[]"
  return ()

parseInstruction = P.choice
  [ parseBack
  , parseForward
  , parseIncrement
  , parseDecrement
  , parseAccept
  , parseEmit
  , parseLoop
  ]

parseInstructions :: P.Parser [Instruction]
parseInstructions = do
  parseComment
  parseInstruction `P.sepEndBy` parseComment




type Cells = M.IntMap Word8
type DataPtr = Int
type Runner = StateT (DataPtr, Cells) IO ()

zeroise :: Maybe Word8 -> Word8
zeroise = fromMaybe 0


runInstruction :: Instruction -> Runner

runInstruction Back      = modify (\(dataPtr, cells) -> (dataPtr - 1, cells))
runInstruction Forward   = modify (\(dataPtr, cells) -> (dataPtr + 1, cells))

runInstruction Increment = do
  (dataPtr, cells) <- get
  let val = zeroise (M.lookup dataPtr cells)
  put (dataPtr, M.insert dataPtr (val + 1) cells)
runInstruction Decrement = do
  (dataPtr, cells) <- get
  let val = zeroise (M.lookup dataPtr cells)
  put (dataPtr, M.insert dataPtr (val - 1) cells)

runInstruction Accept = do
  (dataPtr, cells) <- get
  c               <- liftIO getChar
  put (dataPtr, M.insert dataPtr (fromIntegral (fromEnum c)) cells)
runInstruction Emit = do
  (dataPtr, cells) <- get
  let val = zeroise (M.lookup dataPtr cells)
  liftIO . putChar . toEnum $ fromIntegral val

runInstruction loop@(Loop instructions) = do
  (dataPtr, cells) <- get
  let val = zeroise (M.lookup dataPtr cells)
  case val of
    0 -> return ()
    _ -> runInstructions instructions >> runInstruction loop

runInstructions :: [Instruction] -> Runner
runInstructions = mapM_ runInstruction



run :: Source -> IO ()
run source = case P.parseString parseInstructions mempty source of
  (P.Success instructions) ->
    evalStateT (runInstructions instructions) (0, M.empty)
  (P.Failure e) -> print e


add :: Source
add = "++>+<-"

hw :: Source
hw =
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

fib :: Source
fib =
  "+++++++++++"
    ++ ">+>>>>++++++++++++++++++++++++++++++++++++++++++++"
    ++ ">++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>"
    ++ "+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-"
    ++ "<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<"
    ++ "-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]"
    ++ ">[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++"
    ++ "+++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++"
    ++ "++++++++++++++++++++++++++++++++++++++++++++.[-]<<"
    ++ "<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<"
    ++ "[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"

quine :: Source
quine =
  ">>>>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>+>+>+>+>+>+>+>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++++>+>+>+>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++++>+>+>++++++++++++++++++++>+>+>+>++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>+>++++++++++++++++++++>+++>++++>++++>++++>++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++>++++++++++++++++++>++++>++++++++++++++++++>+>++++++++++++++++++++>++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++++>+>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++++++++++++++++>+>++++++++++++++++++++>++++++++++++++++++++>+>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++>+++>++++>++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++><[<]<<<+++++++[->+++<]>[->++>+++<<]>+>-....>[[-<<.<+>>>]<.[->+<]<[-<+>>+<]>>>]<<<<[<]>[-.>]"

