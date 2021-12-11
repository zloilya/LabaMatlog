{-# LANGUAGE DerivingStrategies #-}
module Expression (Binop(..), ExprEmpty(..), Expr(..), Goal(..)) where

data Binop = Impl | Or | And deriving (Eq, Ord)

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

-- отдельные объекты и уже вычесленное подвыражение
data ExprEmpty = BinE Binop
               | BrekE [ExprEmpty]
               | NotE ExprEmpty
               | VarE String
               | Ex Expr 
               deriving stock Show

-- итоговое выражение
data Expr = Binary Binop Expr Expr
          | Not Expr
          | Bottom
          | Var String 
          deriving stock (Eq, Ord) -- естественный

instance Show Expr where
  show (Binary bin a b) = 
   "(" ++ show a ++ ")" ++ show bin ++ "(" ++ show b ++ ")"
  show (Not e)           = show (Binary Impl e Bottom)
  show (Var name)        = name
  show Bottom            = "_|_"

-- хотим уметь печатать, то что надо, а не то как это выглядит
-- версия со скобочками
{-
instance Show Expr where
  show (Binary Impl a b) = 
   "(" ++ show a ++ ")" ++ show Impl ++ show b
  show (Binary And a b) = 
   "(" ++ show a ++ ")" ++ show And ++ "(" ++ show b ++ ")"
  show (Binary Or a b) = 
   "(" ++ show a ++ ")" ++ show Or ++ "(" ++ show b ++ ")"
  show (Not e)           = show (Binary Impl e Bottom)
  show (Var name)        = name
  show Bottom            = "_|_"
-}
-- пралавила дерева + ошибка
data Goal = Fail String          -- 1
          | Gip  Expr            -- 2
          | Cons Expr Goal       -- 3
          | Mp   Expr Goal Goal  -- 4
          | IAnd Expr Goal Goal  -- 5
          | EL   Expr Goal       -- 6
          | ER   Expr Goal       -- 7
          | IL   Expr Goal       -- 8
          | IR   Expr Goal       -- 9
          | E    Expr Goal Goal Goal  -- 10
          | EFal Expr Goal       -- 11

-- Map
instance Eq Goal where
   (==) (Gip e1) (Gip e2)       = e1 == e2
   (==) (Cons e1 _) (Cons e2 _) = e1 == e2
   (==) (Mp e1 _ _) (Mp e2 _ _) = e1 == e2
   (==) (Cons e1 _) (Gip e2)    = e1 == e2
   (==) (Gip e1) (Cons e2 _)    = e1 == e2
   (==) (Mp e1 _ _) (Gip e2)    = e1 == e2
   (==) (Gip e1) (Mp e2 _ _)    = e1 == e2
   (==) (Cons e1 _) (Mp e2 _ _) = e1 == e2
   (==) (Mp e1 _ _) (Cons e2 _) = e1 == e2
   (==) _ _                     = undefined

-- Map
instance Ord Goal where
   (<=) (Gip e1) (Gip e2)       = e1 <= e2
   (<=) (Cons e1 _) (Cons e2 _) = e1 <= e2
   (<=) (Mp e1 _ _) (Mp e2 _ _) = e1 <= e2
   (<=) (Cons e1 _) (Gip e2)    = e1 <= e2
   (<=) (Gip e1) (Cons e2 _)    = e1 <= e2
   (<=) (Mp e1 _ _) (Gip e2)    = e1 <= e2
   (<=) (Gip e1) (Mp e2 _ _)    = e1 <= e2
   (<=) (Cons e1 _) (Mp e2 _ _) = e1 <= e2
   (<=) (Mp e1 _ _) (Cons e2 _) = e1 <= e2
   (<=) _ _                     = undefined

{-
showE :: Expr -> String
showE (Binary bin (Var n) (Var n2)) = 
   n ++ show bin ++ n2
showE (Binary bin a (Var n2)) = 
   "(" ++ showE a ++ ")" ++ show bin ++ n2
showE (Binary bin (Var n) b) = 
   n ++ show bin ++ showE b
showE (Binary bin a b) = 
   "(" ++ showE a ++ ")" ++ show bin ++ showE b
showE (Not e)           = "!" ++ showE e
showE (Var name)        = name
showE Bottom            = "_|_"

-- debug
instance Show Goal where
   show (Fail error)  = error
   show (Gip expr)    = showE expr
   show (Cons expr _) = showE expr
   show (Mp expr _ _) = showE expr
   show _             = undefined
-}
