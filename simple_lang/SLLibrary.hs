module SLLibrary (readExpr,
                  eval,
                  trapError,
                  extractValue,
                  runRepl,
                  evalAndPrint) where

import           SLEvaluator
import           SLParser
import           SLRepl
import           SLTypes
