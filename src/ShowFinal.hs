module ShowFinal where

import qualified Data.Map   as Map
import           Expression (Binop (And, Impl, Or), Expr (..), Goal (..))
import           Help       (getLeft)

mapToList :: Map.Map Expr Goal -> [Goal]
mapToList list = solve (Map.toList list) where
   solve ((a, b) : ls) = b : solve ls
   solve []            = []

showGips :: Goal -> String
showGips (Gip expr) = show expr
showGips _          = ""

showFinal :: (Goal , Map.Map Expr Goal) -> String
showFinal (goal, steck) = solve 0 goal initgips 
  where
   initgips :: String
   initgips = case Map.size steck of 
      0 -> "" 
      _ -> init (concatMap (\a -> showGips a ++ ",") (mapToList steck))
   
   solve :: Int -> Goal -> String -> String
   solve n goal gips = case goal of 
      (Fail error) -> error
      (Gip expr) -> showFoo expr ++ " [Ax]\n"

      (Mp expr left right) -> 
         exprLeftRight expr left right ++ " [E->]\n"
      (IAnd expr left right) -> 
         exprLeftRight expr left right ++ " [I&]\n"
      
      (Cons expr prev) -> 
         solve (n + 1) prev (show (getLeft expr) 
         ++ gipsPlusComma gips) ++ showFoo expr ++ " [I->]\n"
      
      (EL expr prev)   -> exprPrev expr prev ++ " [El&]\n"
      (ER expr prev)   -> exprPrev expr prev ++ " [Er&]\n"
      (IL expr prev)   -> exprPrev expr prev ++ " [Il|]\n"
      (IR expr prev)   -> exprPrev expr prev ++ " [Ir|]\n"
      (EFal expr prev) -> exprPrev expr prev ++ " [E_|_]\n"

      (E expr left right it@(Gip (Binary Or a b))) ->
        solve (n + 1) left  (show a ++ gipsPlusComma gips) ++
        solve (n + 1) right (show b ++ gipsPlusComma gips) ++
        solve (n + 1) it gips ++ showFoo expr ++ " [E|]\n"
      _ -> undefined
     where
       gipsPlusComma :: String -> String
       gipsPlusComma ""   = ""
       gipsPlusComma gips = "," ++ gips

       showSolve :: Int -> String -> Expr -> String 
       showSolve n gips expr = 
         "[" ++ show n ++ "] " ++ gips ++ "|-" ++ show expr

       exprLeftRight expr left right = 
         solve (n + 1) left gips ++ solve (n + 1) right gips ++ showFoo expr
       
       exprPrev expr prev = solve (n + 1) prev gips ++ showFoo expr

       showFoo = showSolve n gips
