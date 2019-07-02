{-# LANGUAGE LambdaCase #-}

-- http://dev.stephendiehl.com/fun/002_parsers.html
module NanoParsec where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)] -> error "Parser did not consume entire stream."
    _ -> error "Parser error."

item :: Parser Char
item =
  Parser $ \case
    [] -> []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) =
    Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option p q =
  Parser $ \s ->
    case parse p s of
      [] -> parse q s
      res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else failure

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

noneOf :: [Char] -> Parser Char
noneOf s = satisfy (`notElem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a =
      (do f <- op
          b <- p
          rest (f a b)) <|>
      return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c : cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  _ <- spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

brackets :: Parser a -> Parser a
brackets m = do
  _ <- reserved "["
  n <- m
  _ <- reserved "]"
  return n
