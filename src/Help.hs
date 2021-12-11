module Help where

import qualified Data.Map   as Map
import           Expression (Binop (And, Impl, Or), Expr (..), Goal (..))

ifGip :: Goal -> Expr
ifGip (Gip e)      = e
ifGip (Cons e _)   = e
ifGip (Mp e _ _)   = e
ifGip (IAnd e _ _) = e
ifGip (EL e _)     = e
ifGip (ER e _)     = e
ifGip (IL e _)     = e
ifGip (IR e _)     = e
ifGip (E e _ _ _)  = e
ifGip (EFal e _)   = e
ifGip (Fail _)     = undefined

getLeft :: Expr -> Expr
getLeft (Binary _ left _) = left
getLeft _                 = undefined

getRight :: Expr -> Expr
getRight (Binary _ _ right) = right
getRight _                  = undefined

goalToExpr :: Goal -> Expr
goalToExpr (Gip e)    = e
goalToExpr (Cons e _) = e
goalToExpr (Mp e _ _) = e
goalToExpr _          = undefined
