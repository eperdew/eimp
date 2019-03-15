{-# LANGUAGE GADTs, ExistentialQuantification, StandaloneDeriving #-}

module Eval
( evalTStmt
, evalTRvalue
, Result
, ResultT
, TContext
, RuntimeError(..)
, TValue(..)
) where

import Ast
import Types hiding (TypeError(..))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map

evalTStmt :: TContext -> TStmt -> ResultT IO TContext
evalTStmt context TSkip = return context
evalTStmt context (TAssign id (e ::: t)) = hoistResult $ do
    v <- evalTRvalue context e
    storeTValue context id $ liftTValue v t
evalTStmt context (TSequence stmts) =
    foldM evalTStmt context stmts
evalTStmt context (TIf e s1 s2) = do
    b <- hoistResult $ evalTRvalue context e
    evalTStmt context (if b then s1 else s2)
evalTStmt context (TWhile e s) = do
    b <- hoistResult $ evalTRvalue context e
    if b then evalTStmt context s >>= flip evalTStmt (TWhile e s)
    else return context
evalTStmt context (TPrint s e) = do
    v <- hoistResult $ evalTRvalue context e
    liftIO $ print (s ++ (show v))
    return context
evalTStmt context (TAssert e s) = hoistResult $ do
    b <- evalTRvalue context e
    if b then return context
    else throwError $ AssertionFailed (e ::: TBoolean) s

evalTRvalue :: TContext -> TRvalue a -> Result a
evalTRvalue context (TInt i)  = return i
evalTRvalue context (TBool b) = return b
evalTRvalue context (TVar id t) = do
    ATValue v t' <- lookupTV context id
    Eq <- eqOrError t t' (id ++ " did not have the expected type")
    return $ extract v
evalTRvalue context (TBinop op l r) = do
    v1 <- evalTRvalue context l
    v2 <- evalTRvalue context r
    return $ liftOperator op v1 v2
evalTRvalue context (TUnop op e) = do
    v <- evalTRvalue context e
    return $ liftOperator op v

data TValue a where
    TIntVal :: Integer -> TValue Integer
    TBoolVal :: Bool -> TValue Bool
deriving instance Show (TValue a)

data ATValue = forall a. ATValue (TValue a) (TType a)
deriving instance Show ATValue

instance Carrier TValue where
    extract (TIntVal  i) = i
    extract (TBoolVal b) = b

eqOrError :: TType a -> TType b -> String -> Result (Equal a b)
eqOrError t1 t2 e = case testEqM t1 t2 of
    Nothing -> throwError $ CompilerError e
    Just witness -> return witness

type TypeContext = Map.Map Identifier Type
type Result a = Either RuntimeError a
type ResultT m a = ExceptT RuntimeError m a

type TContext = Map.Map Identifier ATValue

-- Error types encountered during evaluation
data RuntimeError
    = AssertionFailed ATRvalue String
    | TypeMismatch Type Type
    | CompilerError String
    | UnboundVariable Identifier
    deriving (Show)

-- Helper functions for dealing with contexts

lookupTV :: TContext -> Identifier -> Result ATValue
lookupTV store id = case Map.lookup id store of
    Nothing -> throwError $ UnboundVariable id
    Just t -> return t

storeTValue :: TContext -> Identifier -> ATValue -> Result TContext
storeTValue store id (ATValue v t) = case Map.lookup id store of
    Nothing -> return $ Map.insert id (ATValue v t) store
    Just (ATValue _ t') -> do
        Eq <- eqOrError t t' (id ++ " did not have the expected type")
        return $ Map.insert id (ATValue v t) store

liftTValue :: a -> TType a -> ATValue
liftTValue i t@TInteger = ATValue (TIntVal  i) t
liftTValue b t@TBoolean = ATValue (TBoolVal b) t

hoistResult :: Monad m => Result a -> ResultT m a
hoistResult = ExceptT . return

liftOperator :: TOperator a -> a
liftOperator TPlus       = (+)
liftOperator TMinus      = (-)
liftOperator TTimes      = (*)
liftOperator TDiv        = div
liftOperator TMod        = mod
liftOperator TAnd        = (&&)
liftOperator TOr         = (||)
liftOperator TEqual      = (==)
liftOperator TNotEqual   = (/=)
liftOperator TLess       = (<)
liftOperator TLessEq     = (<=)
liftOperator TGreater    = (>)
liftOperator TGreaterEq  = (>=)
liftOperator TUnaryPlus  = id
liftOperator TUnaryMinus = (0-)
liftOperator TNot        = not

