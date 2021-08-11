module SLEvaluator (eval) where

import           SLParser
import           SLTypes

import           Control.Monad
import           Control.Monad.Except
import           Data.Array
import           Data.Bits
import           Data.Char                     (toLower)
import           Data.Complex
import           Data.Ratio
import           Numeric
import           Text.ParserCombinators.Parsec hiding (spaces)

eval :: LispVal -> ThrowsError LispVal
eval x =
  case x of
    val@(String _) -> return val
    val@(Number _) -> return val
    val@(Bool _) -> return val
    val@(Character _) -> return val
    val@(List ((Atom _) : _)) -> evalAtom val
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

evalAtom :: LispVal -> ThrowsError LispVal
evalAtom x =
  case x of
    List [Atom "quote", val] -> return val
    List [Atom "if", pred, ifExpr, elseExpr] ->
      do result <- eval pred
         case result of
           Bool False -> eval elseExpr
           Bool True  -> eval ifExpr
           badPred    -> throwError $ TypeMismatch "bool" badPred
    form@(List ((Atom "cond") : exprs)) ->
      case exprs of
        [] -> throwError $ BadSpecialForm "No clauses for cond found" form
        _  -> evalCond exprs
    form@(List ((Atom "case") : key : clauses)) ->
      case clauses of
        [] -> throwError $ BadSpecialForm "No clauses for case found" form
        _ -> do result <- eval key
                evalCase result clauses
    List (Atom func : args) -> mapM eval args >>= apply func
    form -> throwError $ TypeMismatch "atom list" form

evalCase :: LispVal -> [LispVal] -> ThrowsError LispVal
evalCase key clauses =
  case clauses of
    [] -> throwError $ BadSpecialForm "No valid clauses for case found; Missing else?" $
          List clauses
    (List ((List datums):[pred])):clauses' ->
      let
        clauseResult = any (\x -> eqPair (key, x)) datums
      in
        if clauseResult
        then eval pred
        else evalCase key clauses'
    (List ((Atom "else"):[pred])):clauses' -> eval pred
    badForm -> throwError $ BadSpecialForm "Invalid clause for case found" $ List badForm

evalCond :: [LispVal] -> ThrowsError LispVal
evalCond exprs =
  case exprs of
    [] -> throwError $ BadSpecialForm "No valid clauses for cond found; Missing else?" $
          List exprs
    expr:exprs' ->
      case expr of
        List [Atom "else", elseExpr] -> eval elseExpr
        List [pred, predExpr] -> do result <- eval pred
                                    case result of
                                      Bool True -> eval predExpr
                                      Bool False -> evalCond exprs'
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
primitives = symbolPrimitives ++
             stringPrimitives ++
             numPrimitives ++
             boolPrimitives ++
             listPrimitives ++
             eqPrimitives

symbolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
symbolPrimitives = [("symbol?", unaryOp isSymbol),
                    ("symbol->string", unaryOp symbolToString)]

stringPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
stringPrimitives = [("string?", unaryOp isString),
                    ("string->symbol", unaryOp stringToSymbol),
                    ("string=?", stringBoolBinOp (==)),
                    ("string>?", stringBoolBinOp (>)),
                    ("string<?", stringBoolBinOp (<)),
                    ("string<=?", stringBoolBinOp (<=)),
                    ("string>=?", stringBoolBinOp (>=)),
                    ("string-ci=?", stringLowerBoolBinOp (==)),
                    ("string-ci>?", stringLowerBoolBinOp (>)),
                    ("string-ci<?", stringLowerBoolBinOp (<)),
                    ("string-ci<=?", stringLowerBoolBinOp (<=)),
                    ("string-ci>=?", stringLowerBoolBinOp (>=)),
                    ("make-string", undefined),
                    ("string", undefined),
                    ("string-ref", undefined),
                    ("substring", undefined),
                    ("string-append", undefined),
                    ("string->list", undefined),
                    ("string-set!", undefined),
                    ("string-copy", undefined),
                    ("string-fill!", undefined)]

numPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numPrimitives = [("number?", unaryOp isNumber),
                 ("+", numericBinOp (+)),
                 ("-", numericBinOp (-)),
                 ("*", numericBinOp (*)),
                 ("/", numericBinOp div),
                 ("mod", numericBinOp mod),
                 ("quotient", numericBinOp quot),
                 ("remainder", numericBinOp rem),
                 ("=", numBoolBinOp (==)),
                 ("<", numBoolBinOp (<)),
                 (">", numBoolBinOp (>)),
                 ("/=", numBoolBinOp (/=)),
                 (">=", numBoolBinOp (>=)),
                 ("<=", numBoolBinOp (<=))]

boolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
boolPrimitives = [("boolean?", unaryOp isBoolean),
                  ("&&", boolBoolBinOp (&&)),
                  ("||", boolBoolBinOp (||))]

listPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
listPrimitives = [("list?", unaryOp isList),
                  ("car", car),
                  ("cdr", cdr),
                  ("cons", cons),
                  ("list->string", undefined)]

eqPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
eqPrimitives = [("eqv?", eqv),
                ("eq?", eq),
                ("equal?", equal)]

eqPair :: (LispVal, LispVal) -> Bool
eqPair (a1, b1) =
  case eqv [a1, b1] of
    Left err         -> False
    Right (Bool val) -> val

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
            [List (x:xs)]         -> return x
            [DottedList (x:xs) _] -> return x
            [singleVal]           -> throwError $ TypeMismatch "pair" singleVal
            wrongNum              -> throwError $ NumArgs 1 wrongNum

cdr :: [LispVal] -> ThrowsError LispVal
cdr lvs = case lvs of
            [List (x:xs)]         -> return $ List xs
            [DottedList [_] y]    -> return y
            [DottedList (_:xs) y] -> return $ DottedList xs y
            [singleVal]           -> throwError $ TypeMismatch "pair" singleVal
            wrongNum              -> throwError $ NumArgs 1 wrongNum

cons :: [LispVal] -> ThrowsError LispVal
cons lvs = case lvs of
             [val, List xs]         -> return $ List (val:xs)
             [val, DottedList xs y] -> return $ DottedList (val:xs) y
             [List [], val2]        -> return $ DottedList [List []] val2
             [List val1, val2]      -> return $ DottedList val1 val2
             [val1, val2]           -> return $ DottedList [val1] val2
             badArgList             -> throwError $ NumArgs 2 badArgList


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
stringLowerBoolBinOp = boolBinOp unpackLowerString
boolBoolBinOp = boolBinOp unpackBool

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = case params of
                           singleVal@[_] -> throwError $ NumArgs 2 singleVal
                           ps -> fmap (Number . foldl1 op) (mapM unpackNum ps)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum x = case x of
                Number n -> return n
                notNum   -> throwError $ TypeMismatch "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString x = case x of
                   String n  -> return n
                   notString -> throwError $ TypeMismatch "string" notString

unpackLowerString :: LispVal -> ThrowsError String
unpackLowerString x = case x of
                        String n -> return $ map toLower n
                        notString -> throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool x = case x of
                 Bool n  -> return n
                 notBool -> throwError $ TypeMismatch "bool" notBool

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f x = case x of
                [val]   -> f val
                badVals -> throwError $ NumArgs 1 badVals

isSymbol, isString, isNumber, isBoolean, isList :: LispVal -> ThrowsError LispVal
isSymbol x = case x of
               Atom _ -> (return . Bool) True
               _      -> (return . Bool) False
isString x = case x of
               String _ -> (return . Bool) True
               _        -> (return . Bool) False
isNumber x = case x of
               Number _ -> (return . Bool) True
               _        -> (return . Bool) False
isBoolean x = case x of
                Bool _ -> (return . Bool) True
                _      -> (return . Bool) False
isList x = case x of
             List _ -> (return . Bool) True
             _      -> (return . Bool) False

symbolToString, stringToSymbol :: LispVal -> ThrowsError LispVal
symbolToString x = case x of
                     Atom s    -> (return . String) s
                     notSymbol -> throwError $ TypeMismatch "symbol" notSymbol
stringToSymbol x = case x of
                     String s  -> (return . Atom) s
                     notString -> throwError $ TypeMismatch "string" notString

