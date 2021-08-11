module Main where

import           Control.Monad
import qualified SLLibrary          (eval, evalAndPrint, extractValue, readExpr,
                                     runRepl, trapError)
import           System.Environment

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> SLLibrary.runRepl
            1 -> (SLLibrary.evalAndPrint . head) args
            _ -> putStrLn "Program only takes 0 or 1 argument"
