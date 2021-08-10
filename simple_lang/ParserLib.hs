module ParserLib (readExpr) where

import Numeric
import Data.Array
import Data.Bits
import Data.Ratio
import Data.Complex
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ (show val)

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
             deriving (Show)

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
                 x <- many $ escapedChars <|> (noneOf "\\\"")
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
               let atom = [first] ++ rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do try $ char '#'
               ((char 't' >> return (Bool True))
                 <|> (char 'f' >> return (Bool False)))

parseNumber :: Parser LispVal
parseNumber = try parseDecNoPrefix
              <|> try parseOct
              <|> try parseHex
              <|> try parseBin
              <|> try parseDec

parseDecNoPrefix :: Parser LispVal
parseDecNoPrefix = liftM (Number . read) $ many1 digit

parseDec :: Parser LispVal
parseDec = do string "#d"
              parseDecNoPrefix

parseOct :: Parser LispVal
parseOct = do string "#o"
              digitStream <- many1 octDigit
              let val = oct2dig digitStream
              return $ Number val
oct2dig x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do string "#b"
              digitStream <- many1 (oneOf "01")
              let val = bin2dig digitStream
              return $ Number val
bin2dig :: String -> Integer
bin2dig = foldl (\acc x -> (shiftL acc 1) + (binChar2dig x)) 0
binChar2dig :: Char -> Integer
binChar2dig = (read . pure)

parseHex :: Parser LispVal
parseHex = do string "#x"
              digitStream <- many1 hexDigit
              let val = hex2dig digitStream
              return $ Number val
hex2dig x = fst $ readHex x !! 0

parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar;
                                      notFollowedBy alphaNum;
                                      return [x]}
                    return $ Character $ case value of
                      "space" -> ' '
                      "newline" -> '\n'
                      otherwise -> (value !! 0)

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
                return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecNoPrefix)
                  char '+'
                  y <- (try parseFloat <|> parseDecNoPrefix)
                  char 'i'
                  let toDouble :: LispVal -> Double
                      toDouble (Number n) = fromIntegral n
                      toDouble (Float f) = realToFrac f
                  return $ Complex (toDouble x :+ toDouble y)

parseAllListTypes :: Parser LispVal
parseAllListTypes = do char '('
                       x <- (try parseList <|> parseDottedList)
                       char ')'
                       return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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
                          let arrLen = (0, (length arrayVals - 1))
                          return $ Vector (listArray arrLen arrayVals)

parseVector :: Parser LispVal
parseVector = do string "#("
                 vec <- parseVectorInternals
                 char ')'
                 return vec
