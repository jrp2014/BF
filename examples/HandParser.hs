module HandParser where

import MachineBF

parse :: Source -> Program
parse [] = []
parse ('+':xs) = Increment : parse xs
parse ('-':xs) = Decrement : parse xs
parse ('<':xs) = Back : parse xs
parse ('>':xs) = Forward : parse xs
parse (',':xs) = Accept : parse xs
parse ('.':xs) = Emit : parse xs
parse ('[':xs) = Loop (parse inner) : parse rest
  where
    (inner, rest) = parseLoop 1 ([], xs)
    parseLoop :: Int -> ([Input], [Input]) -> ([Input], [Input])
    parseLoop 0 x = x
    parseLoop n (x, '[':t) = parseLoop (succ n) (x ++ "[", t)
    parseLoop n (x, ']':t) =
      parseLoop
        (pred n)
        ( if n == 1
            then x
            else x ++ "]"
        , t)
    parseLoop n (x, h:t) = parseLoop n (x ++ [h], t)
    parseLoop _ (_, []) = error "Unterminated loop block"
parse (']':xs) = error $ "Superfluous ']'" ++ xs
parse (_:xs) = parse xs -- ignore other chars
