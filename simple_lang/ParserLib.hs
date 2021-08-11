{-# LANGUAGE ExistentialQuantification #-}
module ParserLib (readExpr, eval, trapError, extractValue) where

import Numeric
import Data.Array
import Data.Bits
import Data.Ratio
import Data.Complex
import Control.Monad
import Control.Monad.Except
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

condAtom :: LispVal
condAtom = Atom "cond"

ifAtom :: LispVal
ifAtom = Atom "if"

quoteAtom :: LispVal
quoteAtom = Atom "quote"

eval :: LispVal -> ThrowsError LispVal
eval x = case x of
           val@(String _) -> return val
           val@(Number _) -> return val
           val@(Bool _) -> return val
           val@(Character _) -> return val
           List [quoteAtom, val] -> return val
           List [Atom "if", pred, ifExpr, elseExpr] -> do result <- eval pred
                                                          case result of
                                                            Bool False -> eval elseExpr
                                                            Bool True -> eval ifExpr
                                                            badPred -> throwError $ TypeMismatch
                                                                       "bool" badPred
           form@(List ((Atom "cond"):exprs)) -> case exprs of
                                                  [] -> throwError $
                                                        BadSpecialForm
                                                        "No clauses for cond found"
                                                        form
                                                  _ -> condEval exprs
           List (Atom func : args) -> mapM eval args >>= apply func
           badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

condEval :: [LispVal] -> ThrowsError LispVal
condEval exprs =
  case exprs of
    [] -> throwError $ BadSpecialForm "No valid clauses for cond found; Missing else?" $
          List exprs
    expr:exprs' -> case expr of
                     List [Atom "else", elseExpr] -> eval elseExpr
                     List [pred, predExpr] -> do result <- eval pred
                                                 case result of
                                                   Bool True -> eval predExpr
                                                   Bool False -> condEval exprs'
                                                   badPred -> throwError $
                                                              TypeMismatch
                                                              "bool"
                                                              badPred
                     badClause -> throwError $ BadSpecialForm
                                  "Malformed clause in cond"
                                  badClause

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
              ("symbol->string", unaryOp symbolToString),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", stringBoolBinOp (==)),
              ("string>?", stringBoolBinOp (>)),
              ("string<?", stringBoolBinOp (<)),
              ("string<=?", stringBoolBinOp (<=)),
              ("string>=?", stringBoolBinOp (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eq),
              ("equal?", equal)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv lvs = case lvs of
            [Bool a, Bool b] -> return $ Bool $ a == b
            [String a, String b] -> return $ Bool $ a == b
            [Number a, Number b] -> return $ Bool $ a == b
            [Character a, Character b] -> return $ Bool $ a == b
            [Atom a, Atom b] -> return $ Bool $ a == b
            [DottedList a b, DottedList x y] -> eqv [List $ a ++ [b], List $ x ++ [y]]
            [List a, List b] -> return $ Bool $ length a == length b &&
                                    let
                                      combList = zip a b
                                      eqPair (a1, b1) = case eqv [a1, b1] of
                                                          Left err -> False
                                                          Right (Bool val) -> val
                                    in
                                      all eqPair combList
            [_, _] -> return $ Bool False
            badArgList -> throwError $ NumArgs 2 badArgList

eq :: [LispVal] -> ThrowsError LispVal
eq = eqv -- FIXME?

equal :: [LispVal] -> ThrowsError LispVal
equal = eqv -- FIXME?

car :: [LispVal] -> ThrowsError LispVal
car lvs = case lvs of
            [List (x:xs)] -> return x
            [DottedList (x:xs) _] -> return x
            [singleVal] -> throwError $ TypeMismatch "pair" singleVal
            wrongNum -> throwError $ NumArgs 1 wrongNum

cdr :: [LispVal] -> ThrowsError LispVal
cdr lvs = case lvs of
            [List (x:xs)] -> return $ List xs
            [DottedList [_] y] -> return y
            [DottedList (_:xs) y] -> return $ DottedList xs y
            [singleVal] -> throwError $ TypeMismatch "pair" singleVal
            wrongNum -> throwError $ NumArgs 1 wrongNum

cons :: [LispVal] -> ThrowsError LispVal
cons lvs = case lvs of
             [val, List xs] -> return $ List (val:xs)
             [val, DottedList xs y] -> return $ DottedList (val:xs) y
             [List [], val2] -> return $ DottedList [List []] val2
             [List val1, val2] -> return $ DottedList val1 val2
             [val1, val2] -> return $ DottedList [val1] val2
             badArgList -> throwError $ NumArgs 2 badArgList
        

boolBinOp :: (LispVal -> ThrowsError a) ->
             (a -> a -> Bool) ->
             [LispVal] ->
             ThrowsError LispVal
boolBinOp unpacker op params = case params of
                                 [leftM, rightM] -> do left <- unpacker leftM
                                                       right <- unpacker rightM
                                                       (return . Bool) $ op left right
                                 wrongNum -> throwError $ NumArgs 2 wrongNum

numBoolBinOp = boolBinOp unpackNum
stringBoolBinOp = boolBinOp unpackString
boolBoolBinOp = boolBinOp unpackBool

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = case params of
                           singleVal@[_] -> throwError $ NumArgs 2 singleVal
                           ps -> fmap (Number . foldl1 op) (mapM unpackNum ps)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum x = case x of
                Number n -> return n
                notNum -> throwError $ TypeMismatch "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString x = case x of
                   String n -> return n
                   notString -> throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool x = case x of
                 Bool n -> return n
                 notBool -> throwError $ TypeMismatch "bool" notBool

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
