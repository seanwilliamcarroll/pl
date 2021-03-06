module SchemeEnvironment (getVar, setVar, defineVar, bindVars) where

import           SchemeTypes

import           Control.Monad.Except
import           Data.IORef
import           Data.Maybe

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                         (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                               (liftIO . flip writeIORef value) (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do valueRef <- newIORef value
                     env <- readIORef envRef
                     writeIORef envRef ((var, valueRef) : env)
                     return value



bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>=
                           extendEnv bindings >>=
                           newIORef where
  extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
  addBinding (var, value) = do ref <- newIORef value
                               return (var, ref)
