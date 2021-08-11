module Main where

import           Control.Monad
import qualified SchemeLibrary      (eval, evalAndPrint, extractValue, readExpr,
                                     runRepl, trapError)
import           System.Environment

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> SchemeLibrary.runRepl
            1 -> (SchemeLibrary.evalAndPrint . head) args
            _ -> putStrLn "Program only takes 0 or 1 argument"
