module Ch1Evaluator (eval) where

import           Ch1Parser
import           Ch1Types

eval :: Env -> Ch1Val -> IOThrowsError Ch1Val
eval env cv =
  case cv of
    form@(Number _) -> return form
    _               -> undefined
