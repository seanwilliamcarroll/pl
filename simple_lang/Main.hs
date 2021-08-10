module Main where

import System.Environment
import qualified MyLib (readExpr)

main :: IO ()
main = do args <- getArgs
          putStrLn (MyLib.readExpr (args !! 0))
