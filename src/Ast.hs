module Ast
( evalAexp
, evalBexp
, evalStmt
, Result
, ResultT
, Store
, Stmt(..)
, Aexp(..)
, Bexp(..)
, ArithBinaryOperator(..)
, ArithUnaryOperator(..)
, BoolBinaryOperator(..)
, BoolUnaryOperator(..)
, Comparator(..)
, RuntimeError(..)
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map

-- Main interpreter - evaluates a (complex) statement in a particular store
evalStmt :: Store -> Stmt -> ResultT IO Store
    -- We're slightly careful here to make sure this is tail recursive
    -- Of note, only a few of these make recursive calls to evalStmt
evalStmt store Skip = liftIO $ return store
evalStmt store (Assign id e) =
    Map.insert id `flip` store <$>
        (hoistResult $ evalAexp store e)
evalStmt store (Sequence stmts) =
    -- By representing a sequence as a list, we can just do a foldM to thread the store
    -- through.
    foldM evalStmt store stmts
evalStmt store (If e s1 s2) =
    do { b <- hoistResult $ evalBexp store e
       ; if b then evalStmt store s1
         else      evalStmt store s2
    }
evalStmt store (While e s) =
    -- while (b) { s } == if (b) { s; while (b) { s } } else { skip }
    evalStmt store $ If e (Sequence [s, While e s]) Skip

-- These two cases represent debug features
evalStmt store (Print s e) =
    do { a <- hoistResult $ evalAexp store e
       ; liftIO $ print (s ++ " " ++ show a)
       ; return store
    }
evalStmt store (Assert e s) =
    do { b <- (hoistResult $ evalBexp store e)
       ; if b then return store
         else throwError $ AssertionFailed e
    }

-- Evaluates an arithmetic expression in a store
-- May return an UnboundVariable RuntimeError
evalAexp :: Store -> Aexp -> Result Integer
evalAexp _ (Int i) = return i
evalAexp s (ArithBinOp op e1 e2) =
    arithBinopToFunc op
    <$> evalAexp s e1
    <*> evalAexp s e2
evalAexp s (ArithUnaryOp op e) =
    arithUnopToFunc  op
    <$> evalAexp s e
evalAexp s (Var id) = case Map.lookup id s of
    Nothing -> Left $ UnboundVariable id
    Just i  -> return i

-- Evaluates a boolean expression in a store
-- May return an UnboundVariable RuntimeError
evalBexp :: Store -> Bexp -> Result Bool
evalBexp _ (Boolean b) = return b
evalBexp s (BoolBinOp op b1 b2) =
    boolBinopToFunc op
    <$> evalBexp s b1
    <*> evalBexp s b2
evalBexp s (BoolUnaryOp op b) =
    boolUnopToFunc op
    <$> evalBexp s b
evalBexp s (Comparison comp a1 a2) =
    boolComparatorToFunc comp
    <$> evalAexp s a1
    <*> evalAexp s a2

-- Main types used by the interpreter
type Identifier = String
type Store = Map.Map String Integer
type Result a = Either RuntimeError a
type ResultT m a = ExceptT RuntimeError m a

hoistResult :: Monad m => Result a -> ResultT m a
hoistResult = ExceptT . return

-- Ast defintions
data Stmt
    = Skip
    | Assign Identifier Aexp
    | Sequence [Stmt]
    | If Bexp Stmt Stmt
    | While Bexp Stmt
    | Print String Aexp
    | Assert Bexp String
    deriving (Show)

-- Arithmetic expressions
data Aexp
    = Int Integer
    | ArithBinOp ArithBinaryOperator Aexp Aexp
    | ArithUnaryOp ArithUnaryOperator Aexp
    | Var Identifier
    deriving (Show, Eq)

-- Boolean expressions
data Bexp
    = Boolean Bool
    | BoolBinOp BoolBinaryOperator Bexp Bexp
    | BoolUnaryOp BoolUnaryOperator Bexp
    | Comparison Comparator Aexp Aexp
    deriving (Show)

-- Operators
data ArithBinaryOperator
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    deriving (Show, Eq)

data ArithUnaryOperator
    = Identity
    | Negate
    deriving (Show, Eq)

data BoolBinaryOperator
    = And
    | Or
    | BoolEqual
    | BoolNotEqual
    deriving (Show, Eq)

data BoolUnaryOperator
    = Not
    deriving (Show, Eq)

data Comparator
    = Less
    | LessEq
    | Greater
    | GreaterEq
    | ArithEqual
    | ArithNotEqual
    deriving (Show, Eq)

-- Error types encountered during evaluation
data RuntimeError
    = UnboundVariable Identifier
    | AssertionFailed Bexp
    deriving (Show)

-- Functions to lift syntactic operators into functions
arithBinopToFunc :: ArithBinaryOperator -> Integer -> Integer -> Integer
arithBinopToFunc Plus  = (+)
arithBinopToFunc Minus = (-)
arithBinopToFunc Times = (*)
arithBinopToFunc Div   = div
arithBinopToFunc Mod   = mod

arithUnopToFunc :: ArithUnaryOperator -> Integer -> Integer
arithUnopToFunc Identity = id
arithUnopToFunc Negate   = (0-)

boolBinopToFunc :: BoolBinaryOperator -> Bool -> Bool -> Bool
boolBinopToFunc And          = (&&)
boolBinopToFunc Or           = (||)
boolBinopToFunc BoolEqual    = (==)
boolBinopToFunc BoolNotEqual = (/=)

boolUnopToFunc :: BoolUnaryOperator -> Bool -> Bool
boolUnopToFunc Not = not

boolComparatorToFunc :: Comparator -> Integer -> Integer -> Bool
boolComparatorToFunc Less          = (<)
boolComparatorToFunc LessEq        = (<=)
boolComparatorToFunc Greater       = (>)
boolComparatorToFunc GreaterEq     = (>=)
boolComparatorToFunc ArithEqual    = (==)
boolComparatorToFunc ArithNotEqual = (/=)
