module ParseObj (parseToObj) where

import           Expression (Binop (..), ExprEmpty (..))

parseToObj :: String -> [ExprEmpty]
parseToObj str@(s : ls) | s == '!'  = headExpr (parseToObj ls)
                        | s == '('  = BrekE (parseToObj ls) : parseToObj (skipB 0 ls)
                        | s == ')'  = []
                        | s == '&'  = BinE And : parseToObj ls
                        | s == '|'  = BinE Or : parseToObj ls
                        | s == '-'  = BinE Impl : parseToObj (tail ls)
                        | otherwise = parseVar str []
parseToObj [] = []

-- у меня не предполагается не валидное выражение,
-- только не верное в плане посылок
headExpr :: [ExprEmpty] -> [ExprEmpty]
headExpr (ps : st) = NotE ps : st
headExpr []        = error "empty Not" -- undefined



skipB :: Int -> String -> String
skipB (-1) ls = ls
skipB acc (s : ls) | s == ')'  = skipB (acc - 1) ls
                   | s == '('  = skipB (acc + 1) ls
                   | otherwise = skipB acc ls
skipB acc [] = []



parseVar :: String -> String -> [ExprEmpty]
parseVar str@(s : ls) st | s /= '!' && s /= '(' && s /= ')'
                        && s /= '&' && s /= '|' && s /= '-'
                        && s /= '>' = parseVar ls (s : st)
                         | otherwise = VarE (reverse st) : parseToObj str
parseVar [] st = [VarE (reverse st)]

