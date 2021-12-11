module Test where

import           MainParser (parser)
import           ShowFinal  (showFinal)
import           Expression 
import           Data.List.Split (splitOn)
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )  

exampleAsk1 :: [String]
exampleAsk1 = [
   "|-A->A->A",
   "A->A->A"]

exampleAsk2 :: [String]
exampleAsk2 = [
   "|-(A->B)->(A->B->C)->(A->C)",
   "(A->B)->(A->B->C)->(A->C)"]

exampleAsk3 :: [String]
exampleAsk3 = [
   "|-A->B->A&B",
   "A->B->A&B"]

exampleAsk4 :: [String]
exampleAsk4 = [
   "|-A&B->B",
   "A&B->B"]

exampleAsk5 :: [String]
exampleAsk5 = [
   "|-A&B->A",
   "A&B->A"]

exampleAsk6 :: [String]
exampleAsk6 = [
   "|-A->A|B",
   "A->A|B"]

exampleAsk7 :: [String]
exampleAsk7 = [
   "|-B->A|B",
   "B->A|B"]

exampleAsk8 :: [String]
exampleAsk8 = [
   "|-(A->C)->(B->C)->(A|B->C)",
   "(A->C)->(B->C)->(A|B->C)"]

exampleAsk9 :: [String]
exampleAsk9 = [
   "|-(A -> B) -> (A -> !B) -> !A",
   "(A -> B) -> (A -> !B) -> !A"]

exampleAsk10 :: [String]
exampleAsk10 = [
   "|-A->!A->B",
   "A->!A->B"]

example1 :: [String]
example1 = [
   "A|-A->A",
   "A->A->A",
   "A",
   "A->A"]

example2 :: [String]
example2 = [
   "A->B, !B |- !A",
   "A->B",
   "!B",
   "!B -> A -> !B",
   "A -> !B",
   "(A -> B) -> (A -> !B) -> !A",
   "(A -> !B) -> !A", "!A"]

example3 :: [String]
example3 = [
   "A,C|-B'",
   "B'"]

example4 :: [String]
example4 = [
   "|- !!A->A",
   "A->B->A",
   "(A->B)->(A->B->C)->(A->C)",
   "A&B->A",
   "A&B->B",
   "A->B->A&B",
   "A->A|B",
   "B->A|B",
   "(A->Q)->(B->Q)->(A|B->Q)",
   "(A->B)->(A->!B)->!A",
   "!!A->A"]

example5 :: [String]
example5 = [
   "|- A -> A",
   "A & A -> A",
   "A -> A -> A",
   "A -> (A -> A) -> A",
   "A & A -> A",
   "(A -> A -> A) -> (A -> (A -> A) -> A) -> A -> A",
   "(A -> (A -> A) -> A) -> A -> A",
   "A & A -> A",
   "A -> A"]

showExample :: [String] -> String
showExample (x : ls) = x ++ "\n" ++ showExample ls
showExample []       = []

showTask :: [String] -> IO ()
showTask task = do
   putStrLn $ "Proof Task"
   putStrLn $ showExample task
   putStrLn $ showFinal (parser task)

test :: IO ()
test = do
   showTask exampleAsk1
   showTask exampleAsk2
   showTask exampleAsk3
   showTask exampleAsk4
   showTask exampleAsk5
   showTask exampleAsk6
   showTask exampleAsk7
   showTask exampleAsk8
   showTask exampleAsk9
   showTask exampleAsk10
   showTask example1
   showTask example2
   showTask example3
   showTask example4
   showTask example5

test5 :: [String]
test5 = [
  "|- A->A",
  "A->(A->A)->A",
  "A->A->A",
  "(A->(A->A))->(A->(A->A)->A)->(A->A)",
  "(A->(A->A)->A)->(A->A)",
  "A->A"]

test6 :: [String]
test6 = [
   "|- A->A",
   "(A->(A->A))->(A->(A->A)->A)->(A->A)", -- акс
   "A->(A->A)->A",                        -- акс
   "A->A->A",                             -- акс
   "(A->(A->A)->A)->(A->A)",              -- mp 4 2
   "A->A"]                                -- mp 3 5

testN :: [String]
testN = [
   "A|B|C->A|(B|C) |- A|B|C->A|(B|C)",
   "(A->(A->A))->(A->(A->A)->A)->(A->A)",
   "( A|B|C->A|(B|C) )"]

testK :: [String]
testK = [
   "|- (A -> B) | (B -> C) -> C",
   "(A->(A->A))->(A->(A->A)->A)->(A->A)",
   "( A|B|C->A|(B|C) )"]

testDenis :: [String]
testDenis = [
   "(A->A)->A |- A",
   "A->(A->A)->A",
   "A->A->A",
   "(A->A->A)->(A->(A->A)->A)->A->A",
   "(A->(A->A)->A)->A->A",
   "A->A",
   "(A->A)->A",
   "A"]

testExpr :: IO ()
testExpr = do
   test
   showTask test5
   showTask test6
   showTask testN
   showTask testK
   showTask testDenis


readOneTest :: String -> IO ()
readOneTest name = do
   handle <- openFile name ReadMode
   contents <- hGetContents handle
   putStrLn contents
   putStrLn "------------------------------------------------"
   putStrLn "my solution"
   putStrLn "------------------------------------------------"
   putStrLn $ showFinal $ parser $ init $ splitOn "\n" contents

oneTestPack1 :: Int -> IO ()
oneTestPack1 num = do
   readOneTest $ "resources/pack1/test" ++ show num ++ "_in.txt"
   putStrLn "------------------------------------------------"
   putStrLn "answer"
   putStrLn "------------------------------------------------"
   handle <- openFile ("resources/pack1/test" ++ show num ++ "_out.txt") ReadMode
   contents <- hGetContents handle
   putStrLn contents

oneTestPack2 :: Int -> IO ()
oneTestPack2 num = do
   readOneTest $ "resources/pack2/test" ++ show num ++ "_in.txt"
   putStrLn "------------------------------------------------"
   putStrLn "answer"
   putStrLn "------------------------------------------------"
   handle <- openFile ("resources/pack2/test" ++ show num ++ "_out.txt") ReadMode
   contents <- hGetContents handle
   putStrLn contents

oneTestPack3 :: Int -> IO ()
oneTestPack3 num = do
   readOneTest $ "resources/pack3/test" ++ show num ++ "_in.txt"
   putStrLn "------------------------------------------------"
   putStrLn "answer"
   putStrLn "------------------------------------------------"
   handle <- openFile ("resources/pack3/test" ++ show num ++ "_out.txt") ReadMode
   contents <- hGetContents handle
   putStrLn contents

readTests :: IO ()
readTests = do
   mapM_ oneTestPack1 [1,2,3,4,5,7,8,9,10]
   mapM_ oneTestPack2 [1 .. 7]
   mapM_ oneTestPack3 [1 .. 10]


a :: Expr
a = Var "A"

b :: Expr
b = Var "B"

c :: Expr
c = Var "C"

ask1 :: Expr
ask1 = Binary Impl a (Binary Impl b a)

ask2 :: Expr
ask2 = Binary Impl
            (Binary Impl a b)
            (Binary Impl
             (Binary Impl a (Binary Impl b c))
             (Binary Impl a c))

ask3 :: Expr
ask3 = Binary Impl a (Binary Impl b (Binary And a b))

ask4 :: Expr
ask4 = Binary Impl (Binary And a b) a

ask5 :: Expr
ask5 = Binary Impl (Binary And a b) b

ask6 :: Expr
ask6 = Binary Impl a (Binary Or a b)

ask7 :: Expr
ask7 = Binary Impl b (Binary Or a b)

ask8 :: Expr
ask8 = Binary Impl
            (Binary Impl a c)
            (Binary Impl
             (Binary Impl b c)
             (Binary Impl (Binary Or a b) c))

ask9 :: Expr
ask9 = Binary Impl 
            (Binary Impl a b) 
            (Binary Impl 
             (Binary Impl a (Not b)) 
             (Not a))

ask10 :: Expr
ask10 = Binary Impl a (Binary Impl (Not a) b)


testShowAsk :: IO ()
testShowAsk = do
   putStrLn $ 
    concatMap (\a -> show a ++ "\n") 
     [ask1, ask2, ask3, ask4, ask5, ask6, ask7, ask8, ask9, ask10]