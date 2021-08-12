module SchemeEvaluator (eval, primitiveBindings) where

import           SchemeEnvironment
import           SchemeParser
import           SchemeTypes

import           Control.Monad
import           Control.Monad.Except
import           Data.Array
import           Data.Bits
import           Data.Char                     (toLower)
import           Data.Complex
import           Data.Maybe
import           Data.Ratio
import           Numeric
import           Text.ParserCombinators.Parsec hiding (spaces)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env x =
  case x of
    val@(String _) -> return val
    val@(Number _) -> return val
    val@(Bool _) -> return val
    val@(Character _) -> return val
    Atom id -> getVar env id
    val@(List ((Atom _) : _)) -> evalAtom env val
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

evalAtom :: Env -> LispVal -> IOThrowsError LispVal
evalAtom env x =
  case x of
    List [Atom "quote", val] -> return val
    List [Atom "if", pred, ifExpr, elseExpr] ->
      do result <- eval env pred
         case result of
           Bool False -> eval env elseExpr
           Bool True  -> eval env ifExpr
           badPred    -> throwError $ TypeMismatch "bool" badPred
    form@(List ((Atom "cond") : exprs)) ->
      case exprs of
        [] -> throwError $ BadSpecialForm "No clauses for cond found" form
        _  -> evalCond env exprs
    form@(List ((Atom "case") : key : clauses)) ->
      case clauses of
        [] -> throwError $ BadSpecialForm "No clauses for case found" form
        _ -> do result <- eval env key
                evalCase env result clauses
    List [Atom "define",  Atom var, form] ->
      eval env form >>= defineVar env var
    List ((Atom "define"):(List ((Atom var) : params) : body)) ->
      makeNormalFunc env params body >>= defineVar env var
    List ((Atom "define"):(DottedList ((Atom var) : params) varargs) : body) ->
      makeVarargs varargs env params body >>= defineVar env var
    List ((Atom "lambda") : (List params) : body) ->
      makeNormalFunc env params body
    List ((Atom "lambda") : (DottedList params varargs) : body) ->
      makeVarargs varargs env params body
    List ((Atom "lambda") : varargs@(Atom _) : body) ->
      makeVarargs varargs env [] body
    List [Atom "set!", Atom var, form] ->
      eval env form >>= setVar env var
    List (function : args) -> do
      func <- eval env function
      argVals <- mapM (eval env) args
      apply func argVals
    form -> throwError $ TypeMismatch "atom list" form

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env key clauses =
  case clauses of
    [] -> throwError $ BadSpecialForm "No valid clauses for case found; Missing else?" $
          List clauses
    (List ((List datums):[pred])):clauses' ->
      let
        clauseResult = any (\x -> eqPair (key, x)) datums
      in
        if clauseResult
        then eval env pred
        else evalCase env key clauses'
    (List ((Atom "else"):[pred])):clauses' -> eval env pred
    badForm -> throwError $ BadSpecialForm "Invalid clause for case found" $ List badForm

evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond env exprs =
  case exprs of
    [] -> throwError $ BadSpecialForm "No valid clauses for cond found; Missing else?" $
          List exprs
    expr:exprs' ->
      case expr of
        List [Atom "else", elseExpr] -> eval env elseExpr
        List [pred, predExpr] -> do result <- eval env pred
                                    case result of
                                      Bool True -> eval env predExpr
                                      Bool False -> evalCond env exprs'
                                      badPred -> throwError $
                                                 TypeMismatch
                                                 "bool"
                                                 badPred
        badClause -> throwError $ BadSpecialForm
                     "Malformed clause in cond"
                     badClause

makeFunc varargs env params body =
  return $ Func (map showLispVal params) varargs body env

makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showLispVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply function args =
  case function of
    PrimitiveFunc func -> liftThrows $ func args
    Func params varargs body closure ->
      if num params /= num args && isNothing varargs
      then throwError $ NumArgs (num params) args
      else liftIO (bindVars closure $ zip params args) >>=
           bindVarArgs varargs >>=
           evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = last <$> mapM (eval env) body
            bindVarArgs arg env =
              case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
    form -> throwError $ TypeMismatch "function" form

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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

