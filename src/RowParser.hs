module RowParser where

import           Data.List.Split (splitOn)
import qualified Data.Map        as Map
import           Expression      (Binop (And, Impl, Or), Expr (..), Goal (..))
import           Help            (getLeft, getRight, goalToExpr, ifGip)
import           ParseExpr       (parse)

-- парсит сырую Goal
getGoal :: String -> Expr
getGoal = parse . right where
   right :: String -> String
   right (a : b : ls) | a == '|' && b == '-' = ls
                    | otherwise            = right (b : ls)
   right _ = undefined

-- акуратно разбираем String на гипотезы
getGips :: String -> [Goal]
getGips = go . left where
   left :: String -> String
   left (a : b : ls) | a == '|' && b == '-' = []
                     | otherwise            = a : left (b : ls)
   left _ = undefined
   go :: String -> [Goal]
   go str = map (Gip . parse) (filter (/="") (splitOn "," str))

listToTuple :: [Goal] -> [(Expr, Goal)]
listToTuple = solve where
   solve (x : ls) = (ifGip x, x) : solve ls
   solve []       = []

-- парсинг рещения из условия
inputParser :: [String] -> (Map.Map Expr Goal, Expr, [Expr], Expr)
inputParser (first : strs) = (gips, goal, check, lastCheck) where
   -- фильтр по пробелам
   firstW = filter (\a -> a /= ' ' && a /= '\n' && a /= '\r')
   --  гипотезы
   gips = Map.fromList (listToTuple (getGips (firstW first)))
   -- цель
   goal = getGoal (firstW first)
   -- ход решения
   check = map (parse . firstW) strs
   -- последяя строчка должна совпадать с целью
   lastCheck = parse (firstW (last strs))
inputParser [] = undefined
