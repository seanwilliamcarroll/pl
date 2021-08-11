module Main where

import System.Environment
import Control.Monad
import qualified SLLibrary (readExpr, eval, trapError, extractValue)

main :: IO ()
main = do args <- getArgs
          let evaled = fmap show $ (SLLibrary.readExpr . head)  args >>= SLLibrary.eval
          putStrLn $ SLLibrary.extractValue $ SLLibrary.trapError evaled
