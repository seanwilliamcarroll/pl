module Main where

import qualified Ch1Library         (runOne, runRepl)

import           System.Environment


main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> Ch1Library.runRepl
            1 -> (Ch1Library.runOne . head) args
            _ -> putStrLn "Program only takes 0 or 1 argument"
