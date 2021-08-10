module ParserLib (readExpr, eval, trapError, extractValue) where

import Numeric
import Data.Array
import Data.Bits
import Data.Ratio
import Data.Complex
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

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
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal
convertSpecialCharacters :: Char -> String
convertSpecialCharacters c = case c of
                               ' ' -> "space"
                               '\n' -> "newline"
                               _ -> [c]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showLispError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

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
extractValue _ = undefined

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseRatio
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseNumber
            <|> try parseCharacter
            <|> try parseBool
            <|> try parseQuoted
            <|> try parseAllListTypes
            <|> try parseQuasiQuoted
            <|> try parseUnQuoteSplicing
            <|> try parseUnQuote
            <|> try parseVector

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\\\""
                 char '"'
                 return $ String x
escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '"' -> x
                    'r' -> '\r'
                    'n' -> '\n'
                    't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do try $ char '#'
               (char 't' >> return (Bool True))
                 <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = try parseDecNoPrefix
              <|> try parseOct
              <|> try parseHex
              <|> try parseBin
              <|> try parseDec

parseDecNoPrefix :: Parser LispVal
parseDecNoPrefix = Number . read <$> many1 digit

parseDec :: Parser LispVal
parseDec = do string "#d"
              parseDecNoPrefix

parseOct :: Parser LispVal
parseOct = do string "#o"
              digitStream <- many1 octDigit
              let val = oct2dig digitStream
              return $ Number val
oct2dig x = fst $ (head . readOct) x

parseBin :: Parser LispVal
parseBin = do string "#b"
              digitStream <- many1 (oneOf "01")
              let val = bin2dig digitStream
              return $ Number val
bin2dig :: String -> Integer
bin2dig = foldl (\acc x -> shiftL acc 1 + binChar2dig x) 0
binChar2dig :: Char -> Integer
binChar2dig = read . pure

parseHex :: Parser LispVal
parseHex = do string "#x"
              digitStream <- many1 hexDigit
              let val = hex2dig digitStream
              return $ Number val
hex2dig x = fst $ (head . readHex) x

parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar;
                                      notFollowedBy alphaNum;
                                      return [x]}
                    return $ Character $ case value of
                      "space" -> ' '
                      "newline" -> '\n'
                      _ -> head value

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                let [(val, _)] = readFloat $ x ++ "." ++ y
                return $ Float val

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do x <- try parseFloat <|> parseDecNoPrefix
                  char '+'
                  y <- try parseFloat <|> parseDecNoPrefix
                  char 'i'
                  let toDouble :: LispVal -> Double
                      toDouble (Number n) = fromIntegral n
                      toDouble (Float f) = realToFrac f
                  return $ Complex (toDouble x :+ toDouble y)

parseAllListTypes :: Parser LispVal
parseAllListTypes = do char '('
                       x <- try parseList <|> parseDottedList
                       char ')'
                       return x

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =  do head <- endBy parseExpr spaces
                      tail <- char '.' >> spaces >> parseExpr
                      return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do char '`'
                      x <- parseExpr
                      return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do string ",@"
                          x <- parseExpr
                          return $ List [Atom "unquote-splicing", x]

parseVectorInternals :: Parser LispVal
parseVectorInternals = do arrayVals <- sepBy parseExpr spaces
                          let arrLen = (0, length arrayVals - 1)
                          return $ Vector (listArray arrLen arrayVals)

parseVector :: Parser LispVal
parseVector = do string "#("
                 vec <- parseVectorInternals
                 char ')'
                 return vec

eval :: LispVal -> ThrowsError LispVal
eval x = case x of
           val@(String _) -> return val
           val@(Number _) -> return val
           val@(Bool _) -> return val
           List [Atom "quote", val] -> return val
           List (Atom func : args) -> mapM eval args >>= apply func
           badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("symbol?", unaryOp isSymbol),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("boolean?", unaryOp isBoolean),
              ("list?", unaryOp isList),
              ("string->symbol", unaryOp stringToSymbol),
              ("symbol->string", unaryOp symbolToString)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = case params of
                           singleVal@[_] -> throwError $ NumArgs 2 singleVal
                           ps -> mapM unpackNum ps >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum x = case x of
                Number n -> return n
                notNum -> throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f x = case x of
                [val] -> f val
                badVals -> throwError $ NumArgs 1 badVals

isSymbol, isString, isNumber, isBoolean, isList :: LispVal -> ThrowsError LispVal
isSymbol x = case x of
               Atom _ -> (return . Bool) True
               _ -> (return . Bool) False
isString x = case x of
               String _ -> (return . Bool) True
               _ -> (return . Bool) False
isNumber x = case x of
               Number _ -> (return . Bool) True
               _ -> (return . Bool) False
isBoolean x = case x of
                Bool _ -> (return . Bool) True
                _ -> (return . Bool) False
isList x = case x of
             List _ -> (return . Bool) True
             _ -> (return . Bool) False

symbolToString, stringToSymbol :: LispVal -> ThrowsError LispVal
symbolToString x = case x of
                     Atom s -> (return . String) s
                     notSymbol -> throwError $ TypeMismatch "symbol" notSymbol
stringToSymbol x = case x of
                     String s -> (return . Atom) s
                     notString -> throwError $ TypeMismatch "string" notString
