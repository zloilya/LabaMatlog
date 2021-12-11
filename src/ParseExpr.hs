module ParseExpr (parse) where

import           Expression  (Binop (And, Impl, Or),
                              Expr (Binary, Not, Var), ExprEmpty (..))
import           ParseObj    (parseToObj)

parse :: String -> Expr
parse str = solveImpl (parseToObj str) []

-- берем первый верхнюю Impl(->)
-- и затем рекурсивно вычисляем его правого и левого соседа
solveImpl :: [ExprEmpty] -> [ExprEmpty] -> Expr
solveImpl ((BinE Impl) : ls) st = Binary Impl 
                                  (solveImpl (reverse st) []) 
                                  (solveImpl ls [])
solveImpl (s : ls)           st = solveImpl ls (s : st)
solveImpl []                 st = solve (reverse st) where
  -- если ничего не нашли идем парсить обычно or and выражение
  solve :: [ExprEmpty] -> Expr
  solve ls = solveOr (reverse (solveAnd ls []))

-- ищем умножение (&) и только затем парсим (|)
solveAnd :: [ExprEmpty] -> [ExprEmpty] -> [ExprEmpty]
solveAnd = start where
  -- элементы должны находится по 3 или 1
  start :: [ExprEmpty] -> [ExprEmpty] -> [ExprEmpty]
  start []               _  = [] -- error "nonEmpty" ???
  start (a : b : c : ls) st = go a b c ls st
  start a                st = Ex (emptyToExpr (head a)) : st -- head
  -- нашли 3 элемента и давайте
  go :: ExprEmpty -> ExprEmpty -> ExprEmpty 
     -> [ExprEmpty] -> [ExprEmpty] -> [ExprEmpty]
  go fst (BinE And)  sec ls st = 
    start (Ex (Binary And (emptyToExpr fst) (emptyToExpr sec)) : ls) st
  go fst b@(BinE Or) sec ls st = 
    start (sec : ls) (b : Ex (emptyToExpr fst) : st)
  go _   _ _   _  _  = error "why is Impl?" -- undefined
  -- кажется такое может произойти, но я честно хз, что тогда делать

solveOr :: [ExprEmpty] -> Expr
solveOr = emptyToExpr . head . start where -- head
  start :: [ExprEmpty] -> [ExprEmpty]
  -- не осталось (->) или (&) поэтому только (|) осталось смерджить
  start (fst : _ : sec : ls) = 
    start (Ex (Binary Or (emptyToExpr fst) (emptyToExpr sec)) : ls)
  start a                    = a

-- частичное или финальное вычилсение
emptyToExpr :: ExprEmpty -> Expr
emptyToExpr (Ex expr)   = expr
emptyToExpr (VarE name) = Var name
emptyToExpr (BrekE ps)  = solveImpl ps []
emptyToExpr (NotE ps)   = Not (emptyToExpr ps)
emptyToExpr (BinE _)    = error "emptyToExpr" -- undefined
