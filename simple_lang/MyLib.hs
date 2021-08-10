module MyLib (readExpr) where

import Numeric
import Data.Bits
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
             deriving (Show)

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
parseNumber = try parseBareNumber
              <|> try parseOct
              <|> try parseHex
              <|> try parseBin
              <|> try parseDec

parseBareNumber :: Parser LispVal
parseBareNumber = liftM (Number . read) $ many1 digit

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

parseDec :: Parser LispVal
parseDec = do string "#d"
              parseBareNumber

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseFloat
  <|> try parseNumber
  <|> try parseCharacter
  <|> try parseBool
