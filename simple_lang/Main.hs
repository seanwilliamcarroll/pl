module Main where

import System.Environment
import Control.Monad
import qualified ParserLib (readExpr, eval, trapError, extractValue)

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ (ParserLib.readExpr . head)  args >>= ParserLib.eval
          putStrLn $ ParserLib.extractValue $ ParserLib.trapError evaled
