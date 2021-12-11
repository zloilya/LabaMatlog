module Main where

import           Data.List.Split (splitOn)
import           MainParser      (parser)
import           ShowFinal       (showFinal)
import           Test            (testExpr, readTests, testShowAsk)

main :: IO ()
main = do
   --readTests   
   --testExpr
   --testShowAsk
   contents <- getContents
   putStrLn $ showFinal $ parser $ init $ splitOn "\n" contents

