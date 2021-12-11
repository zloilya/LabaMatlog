module MainParser where

import           Control.Applicative (Alternative ((<|>)))
--import           Debug.Trace         (trace)
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import qualified Data.Set            as Set
import           Expression          (Binop (And, Impl, Or), Expr (..),
                                      Goal (..))
import           Help                (getLeft, getRight, goalToExpr, ifGip)
import           ParseExpr           (parse)
import           RowParser           (inputParser)

-- проверка не гипотеза ли это/
isGip :: Expr -- предпологаемая гипотеза
      -> Map.Map Expr Goal -- мапа со существующими гипотезами
      -> Maybe Goal -- maybe result
isGip = Map.lookup

isAsk :: Expr -> Maybe Goal
isAsk m = ask1 m <|> ask2 m <|> ask3 m <|> ask4 m <|> ask5 m
      <|> ask6 m <|> ask7 m <|> ask8 m <|> ask9 m <|> ask10 m where
   ask1 :: Expr -> Maybe Goal
   -- (a) -> (b) -> a1 
   -- a --> (b --> a)
   -- | a == a1
   ask1 it@(Binary Impl a (Binary Impl b a1))
    | a == a1 = Just $
      Cons it (Cons step1 (Gip step2)) where
         step1 = getRight it
         step2 = getRight step1
   ask1 _ = Nothing
   ask2 :: Expr -> Maybe Goal
   -- (a -> b1) -> ((a1) -> (b) -> y) -> (a2) -> y2
   -- (a --> b) --> ((a --> (b --> c)) --> (a --> c))
   -- | a == a1 & a1 == a2 & b == b1 & y == y2 
   ask2 it@(Binary Impl
            (Binary Impl a b1)
            (Binary Impl
             (Binary Impl a1 (Binary Impl b y))
             (Binary Impl a2 y2)))
    | a == a1 && a1 == a2 && b == b1 && y == y2 = Just $
      Cons it
       (Cons step1
        (Cons step2
         (Mp step3
          (Mp step4_1 (Gip step4_1_1) (Gip step4_1_2))
          (Mp step4_2 (Gip step4_2_1) (Gip step4_2_2))))) where
         step1 = getRight it
         step2 = getRight step1
         step3 = getRight step2
         step4_1 = Binary Impl b y
         step4_2 = b
         step4_1_1 = Binary Impl a step4_1
         step4_1_2 = a
         step4_2_1 = Binary Impl a step4_2
         step4_2_2 = a
   ask2 _ = Nothing
   ask3 :: Expr -> Maybe Goal
   -- (a) -> (b) -> (a1) & (b1)
   -- a --> (b --> (a Grammar.&& b))
   ask3 it@(Binary Impl a (Binary Impl b (Binary And a1 b1)))
    | a == a1 && b == b1 = Just $
      Cons it (Cons step1 (IAnd step2 (Gip step3_2) (Gip step3_1))) where
         step1 = getRight it
         step2 = getRight step1
         step3_1 = getRight step2
         step3_2 = getLeft  step2
   ask3 _ = Nothing
   ask4 :: Expr -> Maybe Goal
   -- ((a) & (b)) -> a1
   -- (a Grammar.&& b) --> a
   ask4 it@(Binary Impl (Binary And a b) a1) | a == a1 = Just $
      Cons it (EL step1 (Gip step2)) where
         step1 = getRight it
         step2 = Binary And a b
   ask4 _ = Nothing
   ask5 :: Expr -> Maybe Goal
   -- ((a) & (b)) -> b1
   -- (a Grammar.&& b) --> b
   ask5 it@(Binary Impl (Binary And a b) b1) | b == b1 = Just $
      Cons it (ER step1 (Gip step2)) where
         step1 = getRight it
         step2 = Binary And a b
   ask5 _ = Nothing
   ask6 :: Expr -> Maybe Goal
   -- (a) -> (a1) | (b1)
   --  a --> (a Grammar.|| b)
   ask6 it@(Binary Impl a (Binary Or a1 b1)) | a == a1 = Just $
      Cons it (IL step1 (Gip step2)) where
         step1 = getRight it
         step2 = a
   ask6 _ = Nothing
   ask7 :: Expr -> Maybe Goal
   -- (b) -> (a1) | (b1)
   --  b --> (a Grammar.|| b),
   ask7 it@(Binary Impl b (Binary Or a1 b1)) | b == b1 = Just $
      Cons it (IR step1 (Gip step2)) where
         step1 = getRight it
         step2 = b
   ask7 _ = Nothing
   ask8 :: Expr -> Maybe Goal
   -- ((a) -> y) -> ((b) -> y1) -> ((a2) | (b2)) -> y2
   -- (a --> c) --> ((b --> c) --> ((a Grammar.|| b) --> c)),
   ask8 it@(Binary Impl
            (Binary Impl a y)
            (Binary Impl
             (Binary Impl b y1)
             (Binary Impl (Binary Or a2 b2) y2)))
    | a == a2 && b == b2 && y == y1 && y1 == y2 = Just $
      Cons it
       (Cons step1
        (Cons step2
         (E step3
          (Mp step3 (Gip step5_1) (Gip step6_1))
          (Mp step3 (Gip step5_2) (Gip step6_2))
          (Gip step4)))) where
         step1 = getRight it
         step2 =  getRight step1
         step3 = getRight step2
         step4 = Binary Or a b
         step5_1 = Binary Impl a y
         step5_2 = Binary Impl b y
         step6_1 = a
         step6_2 = b
   ask8 _ = Nothing
   ask9 :: Expr -> Maybe Goal
   -- ((a) -> b) -> ((a1) -> !b1) -> !a2
   -- (a --> b) --> ((a --> Grammar.not b) --> Grammar.not a),
   ask9 it@(Binary Impl 
            (Binary Impl a b) 
            (Binary Impl 
             (Binary Impl a1 (Not b1)) 
             (Not a2))) 
    | a == a1 && a1 == a2 && b == b1 = Just $
      Cons it 
       (Cons step1 
        (Cons step2 
         (Mp step3 
          (Mp step4_1 (Gip step5_1_1) (Gip step5_1_2)) 
          (Mp step4_2 (Gip step5_2_1) (Gip step5_2_2))))) where
         step1 = getRight it
         step2 = Binary Impl a Bottom
         step3 = Bottom
         step4_1 = Binary Impl b Bottom
         step4_2 = b
         step5_1_1 = Binary Impl a step4_1
         step5_1_2 = a
         step5_2_1 = Binary Impl a step4_2
         step5_2_2 = a
   ask9 _ = Nothing
   ask10 :: Expr -> Maybe Goal
   -- (a) -> (!a) -> b
   -- a --> (Grammar.not a --> b)
   ask10 it@(Binary Impl a (Binary Impl (Not a1) b)) 
    | a == a1 = Just $
      Cons it 
       (Cons step1 
        (EFal step2 
         (Mp step3 (Gip step4_1) (Gip step4_2)))) where
         step1 = Binary Impl ( Binary Impl a Bottom) b
         step2 = getRight step1
         step3 = Bottom
         step4_1 = Binary Impl a step3
         step4_2 = a
   ask10 _ = Nothing

-- проверяет среди пар, что для какой-то уже нашелся и она нам подходит
isMp :: Expr -- может, ли быть что для нас уже есть пара в mp
     -> Map.Map Expr Goal --
     -> Map.Map Expr (Set.Set (Expr, Goal)) -- тут лежат пары для модус поненса
     -> Maybe Goal
isMp expr steck mp = case Map.lookup expr mp of -- есть ли такой сет с парами?
  Nothing    -> Nothing -- на нет и суда нет
  (Just set) -> case Set.toList $ Set.filter inSteck set of -- inSteck
    ((fs, big) : _) -> Just (Mp expr big (unSafeJust (Map.lookup fs steck)))
    [] -> Nothing
  where
   -- проверяем на готовность для применения Set (Expr, Goal)
   inSteck :: (Expr, Goal) -> Bool
   inSteck (e, _) = isJust $ isGip e steck
   -- после inSteck нагло берем из Just
   unSafeJust :: Maybe Goal -> Goal
   unSafeJust (Just a) = a
   unSafeJust Nothing  = undefined

insertMP :: Goal -- вставляем Expr, для будущего поиска в mp
        -> Map.Map Expr (Set.Set (Expr, Goal)) -- пары для модус поненса
        -> Map.Map Expr (Set.Set (Expr, Goal)) -- пары + 1 для модус поненса
insertMP it mp = case ifGip it of
  (Binary Impl a b) -> case Map.lookup b mp of
    (Just set) -> Map.insert b (Set.insert ( a, it) set) mp
    Nothing    -> Map.insert b (Set.insert ( a, it) Set.empty) mp
  _                 -> mp -- у гипотезы может не быть Impl

insertSteck :: Goal
            -> Map.Map Expr Goal
            -> Map.Map Expr Goal
insertSteck gip =
  Map.insert (ifGip gip) gip

-- давайте попытаемся внести ясность
-- мы возвращем полученную цель и изначальные гепотезы
parser :: [String] -- входное еще необработанное решение
       -> (Goal, Map.Map Expr Goal) -- цель и гипотезы к нему
parser row = case isAsk goal of
  (Just ask) -> (ask, gips)
  Nothing    -> (checkStep check gips Map.empty, gips)
  where
    (gips, goal, check, lastCheck) = inputParser row
    lastLen = length row - 1
    -- проверяем каждый шаг нашего доказательства
    checkStep :: [Expr] -- выражение для каждого шага
       -> Map.Map Expr Goal -- стек с гипотезами
       -> Map.Map Expr (Set.Set (Expr, Goal)) -- пары для модус понанса
       -> Goal -- я хочу вернуть итоговое решение через Goal
    checkStep ls steck mp = case ls of
      [state] -> if goal == lastCheck then winGoal
        else Fail "The proof does not prove the required expression\n"
        where (_, _, winGoal) = isGAM state 0
      (state : str) -> case fail of 
        (Fail str) -> Fail str
        _ -> checkStep str nS nM
        where (nS, nM, fail) = isGAM state (length str)
      [] -> undefined
      where
       isGAM :: Expr -- надо определить её происхождение
             -> Int  -- номер строки
             -> (Map.Map Expr Goal
             , Map.Map Expr (Set.Set (Expr, Goal))
             , Goal) -- решение или Fail
       isGAM state lengthStr = case isGip state steck of
         (Just gip) -> (steck, insertMP gip mp, gip)
         Nothing    -> case isAsk state <|> isMp state steck mp of
           (Just gip) -> (insertSteck gip steck, insertMP gip mp, gip)
           Nothing    -> (steck, mp, Fail $ "Proof is incorrect at line " ++ 
            show (lastLen - lengthStr + 1) ++ "\n")
