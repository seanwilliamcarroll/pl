module Main where

import System.Environment
import Control.Monad
import qualified ParserLib (readExpr, eval, trapError, extractValue)

main :: IO ()
main = do args <- getArgs
          let evaled = fmap show $ (ParserLib.readExpr . head)  args >>= ParserLib.eval
          putStrLn $ ParserLib.extractValue $ ParserLib.trapError evaled
