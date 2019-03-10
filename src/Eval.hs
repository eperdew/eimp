module Eval
( evalStmt
, evalRvalue
, Result
, ResultT
, Store
, TypeContext
, Context
, emptyContext
, RuntimeError(..)
, Value(..)
) where

import Ast
import Types hiding (TypeError(..))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map

-- Main interpreter
evalStmt :: Context -> Stmt Type -> ResultT IO Context
evalStmt context (Skip _) = return context
evalStmt context (Declaration _ id t) = hoistResult $ storeType context id t
evalStmt context (Assign _ id e) =
    hoistResult $ do { v <- evalRvalue context e
                     ; storeValue context id v }
evalStmt context (Sequence _ stmts) =
    foldM evalStmt context stmts
evalStmt context (If _ e s1 s2) =
    do { b <- hoistResult $ evalRvalue context e
       ; case b of
            BoolVal b -> evalStmt context (if b then s1 else s2)
            _ -> throwError $ CompilerError "Condition not a boolean" }
evalStmt context (While x e s) =
    let (types, store) = context in
    do { b <- hoistResult $ evalRvalue context e
       ; case b of
            BoolVal b -> if b
                then evalStmt context s >>=
                    -- We do something a little weird here - we split the context and use
                    -- the old type context with the new store. That's because we don't
                    -- want to push declarations into the typing context more than once.
                    -- TODO: We can probably do something cleaner than this
                    \(_, store') -> evalStmt (types, store') (While x e s)
                else return context
            _ -> throwError $ CompilerError "Condition is not a boolean" }
evalStmt context (Print _ s e) =
    do { v <- hoistResult $ evalRvalue context e
       ; liftIO $ print (s ++ (show v))
       ; return context }
evalStmt context (Assert _ e s) =
    hoistResult $
        do { b <- evalRvalue context e
           ; case b of
                BoolVal True -> return $ context
                BoolVal False -> throwError $ AssertionFailed (const () <$> e)
                _ -> throwError $ CompilerError "Condition not a boolean" }

-- Expression evaluator
evalRvalue :: Context -> (Rvalue a) -> Result Value
evalRvalue ctx (Int _ i) = return $ IntVal i
evalRvalue ctx (Boolean _ b) = return $ BoolVal b
evalRvalue ctx (Var _ id) = lookupV ctx id
evalRvalue ctx (Binop _ op e1 e2) =
    do { v1 <- evalRvalue ctx e1
       ; v2 <- evalRvalue ctx e2
       ; evalBinop op v1 v2 }
evalRvalue ctx (Unop _ op e) =
    do { v <- evalRvalue ctx e
       ; evalUnop op v }

-- Possible things stored in the store
data Value
    = IntVal  Integer
    | BoolVal Bool
    deriving (Show)

typeof :: Value -> Type
typeof (IntVal  _) = IntegerT
typeof (BoolVal _) = BooleanT

type TypeContext = Map.Map Identifier Type
type Store = Map.Map Identifier Value
type Context = (TypeContext, Store)
type Result a = Either RuntimeError a
type ResultT m a = ExceptT RuntimeError m a

emptyContext :: Context
emptyContext = (Map.empty, Map.empty)

-- Error types encountered during evaluation
data RuntimeError
    = AssertionFailed (Rvalue ())
    | TypeMismatch Type Type
    | CompilerError String
    | UnboundVariable Identifier
    deriving (Show)


-- Helper functions for dealing with contexts

lookupT :: Context -> Identifier -> Result Type
lookupT (types,_) id = case Map.lookup id types of
    Nothing -> throwError $ UnboundVariable id
    Just t -> return t

lookupV :: Context -> Identifier -> Result Value
lookupV (_,store) id = case Map.lookup id store of
    Nothing -> throwError $ UnboundVariable id
    Just t -> return t

storeValue :: Context -> Identifier -> Value -> Result Context
storeValue ctx id v =
    let (types, store) = ctx in
    do { t <- lookupT ctx id
       ; let t' = typeof v in
         if t /= typeof v then throwError $ TypeMismatch t t'
         else return $ (types, Map.insert id v store) }

storeType :: Context -> Identifier -> Type -> Result Context
storeType ctx id t =
    let (types, store) = ctx in
    if Map.member id types then throwError $
        CompilerError ("Duplicate binding for variable " ++ id)
    else return $ (Map.insert id t types, store)

hoistResult :: Monad m => Result a -> ResultT m a
hoistResult = ExceptT . return

-- Helper functions for lifting haskell functions to functions of values

liftIntBinop :: (Integer -> Integer -> Integer) -> Value -> Value -> Result Value
liftIntBinop f (IntVal i1) (IntVal i2) = return $ IntVal (f i1 i2)
liftIntBinop _ _ _ = throwError $ CompilerError "Bad arguments to int binop"

liftBoolBinop :: (Bool -> Bool -> Bool) -> Value -> Value -> Result Value
lifeBoolBinop f (BoolVal b1) (BoolVal b2) = return $ BoolVal (f b1 b2)
liftBoolBinop _ _ _ = throwError $ CompilerError "Bad arguments to bool binop"

liftComparison :: (Integer -> Integer -> Bool) -> Value -> Value -> Result Value
liftComparison f (IntVal i1) (IntVal i2) = return $ BoolVal (f i1 i2)
liftComparison _ _ _ = throwError $ CompilerError "Bad arguments to comparison"

liftIntUnop :: (Integer -> Integer) -> Value -> Result Value
liftIntUnop f (IntVal i) = return $ IntVal (f i)
liftIntUnop _ _ = throwError $ CompilerError "Bad arguments to int unop"

liftBoolUnop :: (Bool -> Bool) -> Value -> Result Value
liftBoolUnop f (BoolVal b) = return $ BoolVal (f b)
liftBoolUnop _ _ = throwError $ CompilerError "Bad arguments to bool unop"

valEq :: Value -> Value -> Result Value
valEq (IntVal i1)  (IntVal i2)  = return $ BoolVal (i1 == i2)
valEq (BoolVal b1) (BoolVal b2) = return $ BoolVal (b1 == b2)
valEq _ _ = throwError $ CompilerError "Bad arguments to equals"

valNeq :: Value -> Value -> Result Value
valNeq a b = do { v <- valEq a b
                ; case v of
                    BoolVal b -> return $ BoolVal (not b)
                    _ -> throwError $ CompilerError "valEq gave back an unexpected type" }

-- Lifting functions

evalBinop :: BinaryOperator -> Value -> Value -> Result Value
-- Arithmetic binops
evalBinop Plus  = liftIntBinop (+)
evalBinop Minus = liftIntBinop (-)
evalBinop Times = liftIntBinop (*)
evalBinop Div   = liftIntBinop div
evalBinop Mod   = liftIntBinop mod
-- Boolean binops
evalBinop And = liftBoolBinop (&&)
evalBinop Or  = liftBoolBinop (||)
-- Equals
evalBinop Equal    = valEq
evalBinop NotEqual = valNeq
    -- TODO: Find a way to abstract over these more nicely
-- Comparisons
evalBinop Less      = liftComparison (<)
evalBinop LessEq    = liftComparison (<=)
evalBinop Greater   = liftComparison (>)
evalBinop GreaterEq = liftComparison (>=)

evalUnop :: UnaryOperator -> Value -> Result Value
evalUnop UnaryPlus  = liftIntUnop id
evalUnop UnaryMinus = liftIntUnop (0-)
evalUnop Not = liftBoolUnop not
