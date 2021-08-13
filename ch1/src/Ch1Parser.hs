module Ch1Parser (readExpr) where

import           Ch1Types

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError Ch1Val
readExpr input = case parse parseExpr "ch1lang" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

parseExpr :: Parser Ch1Val
parseExpr = try parseNumber
            <|> try parseAppOrFunDef
            <|> try parseVariable

parseNumber :: Parser Ch1Val
parseNumber = Number . read <$> many1 digit

parseAppOrFunDef :: Parser Ch1Val
parseAppOrFunDef =
  do char '('
     val <- try parseFunDef <|> try parseApplication
     char ')'
     return val

parseApplication :: Parser Ch1Val
parseApplication =
  do function <- parseName
     option () spaces -- Probably better way to do this?
     exprs <- option [] (sepBy parseExpr spaces)
     return $ Application function exprs

parseFunDef :: Parser Ch1Val
parseFunDef =
  do string "define"
     spaces
     function <- parseName
     spaces
     arglist <- parseArgList
     spaces
     expression <- parseExpr
     return $ FunDef function arglist expression

parseArgList :: Parser [String]
parseArgList = do char '('
                  args <- sepBy parseName spaces
                  char ')'
                  return args

parseVariable :: Parser Ch1Val
parseVariable =
  do name <- parseName
     return $ Variable name

parseName :: Parser String
parseName = many1 (alphaNum <|> oneOf "+-*/=<>")






