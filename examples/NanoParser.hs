module NanoParser where

import MachineBF

import Control.Applicative
import Data.Char
import Data.Monoid

import NanoParsec

parser :: Parser Program
parser = do
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
  p <- brackets parser
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
