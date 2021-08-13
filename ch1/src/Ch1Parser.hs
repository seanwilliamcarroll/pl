module Ch1Parser (readExpr) where

import           Ch1Types

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> ThrowsError Ch1Val
readExpr input = case parse parseExpr "ch1lang" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

parseExpr :: Parser Ch1Val
parseExpr = try parseNumber
            <|> try parseList
            <|> try parseAtom

parseNumber :: Parser Ch1Val
parseNumber = Number . read <$> many1 digit

parseList :: Parser Ch1Val
parseList = undefined

parseValueOp :: Parser Ch1Val
parseValueOp = try parseSymbolOp <|> try parsePrintOp

parseSymbolOp :: Parser Ch1Val
parseSymbolOp = do valop <- oneOf "+-*/=<>"
                   return $ ValueOp [valop]

parsePrintOp :: Parser Ch1Val
parsePrintOp = do
  let printStr = "print"
  string printStr
  return $ ValueOp printStr

parseAtom :: Parser Ch1Val
parseAtom = undefined



