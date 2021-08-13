module Ch1Types (Ch1Val(..),
                 Ch1ValError(..),
                 ThrowsError,
                 Env,
                 IOThrowsError,
                 liftThrows,
                 runIOThrows,
                 nullEnv) where

import           Control.Monad
import           Control.Monad.Except
import           Data.IORef
import           System.IO                     hiding (try)
import           Text.ParserCombinators.Parsec hiding (spaces)


data Ch1Val = Number Integer
            | Variable String
            | Application String [Ch1Val]
            | FunDef {funName :: String,
                      formals :: [String],
                      body    :: Ch1Val}


instance Show Ch1Val where show = showCh1Val

showCh1Val :: Ch1Val -> String
showCh1Val cv =
  case cv of
    Number val    -> show val
    Variable var -> "Variable: " ++ show var
    Application funcName vals ->
      "Application: " ++ funcName ++ " applied to args: [" ++
      unwordsList vals ++ "]"
    FunDef {funName = funName,
            formals = formals,
            body = body} ->
      "Function: " ++ funName ++ " with args: [" ++ unwords formals ++ "]"

unwordsList :: [Ch1Val] -> String
unwordsList = unwords . map showCh1Val


data Ch1ValError = NumArgs Integer [Ch1Val]
                 | TypeMismatch String Ch1Val
                 | Parser ParseError
                 | BadSpecialForm String Ch1Val
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

instance Show Ch1ValError where show = showCh1ValError

showCh1ValError :: Ch1ValError -> String
showCh1ValError x =
  case x of
    Parser err -> "Parser error: " ++ show err
    _          -> undefined

type ThrowsError = Either Ch1ValError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue x =
  case x of
    Right val -> val
    _         -> undefined -- Represents a programmer error

type Env = IORef [(String, IORef Ch1Val)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT Ch1ValError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows x =
  case x of
    Left err  -> throwError err
    Right val -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

