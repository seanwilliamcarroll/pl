module Main where

import           Control.Monad
import qualified SLLibrary          (eval, extractValue, readExpr, trapError)
import           System.Environment

main :: IO ()
main = do args <- getArgs
          let evaled = fmap show $ (SLLibrary.readExpr . head)  args >>= SLLibrary.eval
          putStrLn $ SLLibrary.extractValue $ SLLibrary.trapError evaled
