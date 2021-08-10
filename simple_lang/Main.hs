module Main where

import System.Environment
import qualified ParserLib (readExpr)

main :: IO ()
main = do args <- getArgs
          putStrLn (ParserLib.readExpr (args !! 0))
