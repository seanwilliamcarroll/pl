module SchemeTypes (LispVal(..),
                    showLispVal,
                    LispError(..),
                    ThrowsError,
                    trapError,
                    extractValue,
                    Env(..),
                    IOThrowsError(..),
                    nullEnv,
                    liftThrows,
                    runIOThrows) where

import           Control.Monad.Except
import           Data.Array
import           Data.Complex
import           Data.IORef
import           Data.Ratio
import           Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params  :: [String],
                     vararg  :: Maybe String,
                     body    :: [LispVal],
                     closure :: Env}

instance Show LispVal where show = showLispVal

showLispVal :: LispVal -> String
showLispVal lv = case lv of
                   Atom s -> s
                   List l -> "(" ++ unwordsList l ++ ")"
                   DottedList l v -> "(" ++ unwordsList l ++ " . " ++ showLispVal v  ++ ")"
                   Number n -> show n
                   String s -> "\"" ++ s ++ "\""
                   Bool False -> "#f"
                   Bool True -> "#t"
                   Character c -> "#\\" ++ convertSpecialCharacters c
                   Float d -> show d
                   Ratio r -> show r
                   Complex c -> show c
                   Vector v -> show v
                   PrimitiveFunc _ -> "<primitive>" -- FIXME
                   Func {params = args,
                         vararg = varargs,
                         body = body,
                         closure = env} ->
                     "(lambda (" ++ unwords (map show args) ++
                     (case varargs of
                        Nothing  -> ""
                        Just arg -> " . " ++ arg) ++
                     ") ...)"
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal
convertSpecialCharacters :: Char -> String
convertSpecialCharacters c = case c of
                               ' '  -> "space"
                               '\n' -> "newline"
                               _    -> [c]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showLispError

type ThrowsError = Either LispError

showLispError x = case x of
                    NumArgs exp lvs -> "Expected " ++ show exp
                                       ++ " args: found values "
                                       ++ unwordsList lvs
                    TypeMismatch expected lv -> "Invalid type: expected "
                                                ++ expected
                                                ++ ", found "
                                                ++ show lv
                    Parser err -> "Parse error at: " ++ show err
                    BadSpecialForm msg lv -> msg ++ ": " ++ show lv
                    NotFunction msg func -> msg ++ ": " ++ func
                    UnboundVar msg varname -> msg ++ ": " ++ varname

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _           = undefined

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows x =
  case x of
    Left err  -> throwError err
    Right val -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

