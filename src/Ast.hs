module Ast
( evalAexp
, evalBexp
, evalStmt
, evalStmt'
, Store
, Stmt(..)
, Aexp(..)
, Bexp(..)
, A_BinaryOperator(..)
, A_UnaryOperator(..)
, B_BinaryOperator(..)
, B_UnaryOperator(..)
, B_Comparator(..)
) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map

type Identifier = String

data Stmt
    = S_Skip
    | S_Assign Identifier Aexp
    | S_Sequence [Stmt]
    | S_If Bexp Stmt Stmt
    | S_While Bexp Stmt
    | S_Print String Aexp
    | S_Assert Bexp String
    deriving (Show)

data Aexp
    = A_Int Integer
    | A_BinOp A_BinaryOperator Aexp Aexp
    | A_UnaryOp A_UnaryOperator Aexp
    | A_Var Identifier
    deriving (Show, Eq)

data Bexp
    = B_Boolean Bool
    | B_BinOp B_BinaryOperator Bexp Bexp
    | B_UnaryOp B_UnaryOperator Bexp
    | B_Comparison B_Comparator Aexp Aexp
    deriving (Show)

data A_BinaryOperator
    = A_Plus
    | A_Minus
    | A_Times
    deriving (Show, Eq)

data A_UnaryOperator
    = A_Negate
    deriving (Show, Eq)

data B_BinaryOperator
    = B_And
    | B_Or
    | B_BoolEqual
    | B_BoolNotEqual
    deriving (Show, Eq)

data B_UnaryOperator
    = B_Not
    deriving (Show, Eq)

data B_Comparator
    = B_Less
    | B_LessEq
    | B_Greater
    | B_GreaterEq
    | B_ArithEqual
    | B_ArithNotEqual
    deriving (Show, Eq)

arithBinopToFunc :: A_BinaryOperator -> Integer -> Integer -> Integer
arithBinopToFunc A_Plus   = (+)
arithBinopToFunc A_Minus  = (-)
arithBinopToFunc A_Times  = (*)

arithUnopToFunc :: A_UnaryOperator -> Integer -> Integer
arithUnopToFunc A_Negate = (0-)

boolBinopToFunc :: B_BinaryOperator -> Bool -> Bool -> Bool
boolBinopToFunc B_And          = (&&)
boolBinopToFunc B_Or           = (||)
boolBinopToFunc B_BoolEqual    = (==)
boolBinopToFunc B_BoolNotEqual = (/=)

boolUnopToFunc :: B_UnaryOperator -> Bool -> Bool
boolUnopToFunc B_Not = not

boolComparatorToFunc :: B_Comparator -> Integer -> Integer -> Bool
boolComparatorToFunc B_Less          = (<)
boolComparatorToFunc B_LessEq        = (<=)
boolComparatorToFunc B_Greater       = (>)
boolComparatorToFunc B_GreaterEq     = (>=)
boolComparatorToFunc B_ArithEqual    = (==)
boolComparatorToFunc B_ArithNotEqual = (/=)

type Store = Map.Map String Integer

evalAexp :: Aexp -> Store -> Maybe Integer
evalAexp (A_Int i)          _ = Just i
evalAexp (A_BinOp op e1 e2) s =
    arithBinopToFunc op
    <$> evalAexp e1 s
    <*> evalAexp e2 s
evalAexp (A_UnaryOp op e)   s =
    arithUnopToFunc  op
    <$> evalAexp e  s
evalAexp (A_Var id)         s = Map.lookup id s

evalBexp :: Bexp -> Store -> Maybe Bool
evalBexp (B_Boolean b) _ = Just b
evalBexp (B_BinOp op b1 b2) s =
    boolBinopToFunc op
    <$> evalBexp b1 s
    <*> evalBexp b2 s
evalBexp (B_UnaryOp op b) s =
    boolUnopToFunc op
    <$> evalBexp b s
evalBexp (B_Comparison comp a1 a2) s =
    boolComparatorToFunc comp
    <$> evalAexp a1 s
    <*> evalAexp a2 s

evalStmt :: Stmt -> MaybeT IO Store
evalStmt s = evalStmt' s Map.empty

evalStmt' :: Stmt -> Store -> MaybeT IO Store
evalStmt' S_Skip store = liftIO $ return store
evalStmt' (S_Assign id e) store =
    flip (Map.insert id) store <$>
        (MaybeT . return $ evalAexp e store)
evalStmt' (S_Sequence stmts) store =
    foldM (flip evalStmt') store stmts
evalStmt' (S_If e s1 s2) store =
    do { b <- MaybeT . return $ evalBexp e store
       ; if b then evalStmt' s1 store else evalStmt' s2 store
    }
evalStmt' (S_While e s) store =
    evalStmt' (S_If e (S_Sequence [s, S_While e s]) S_Skip) store
evalStmt' (S_Print s e) store =
    do { a <- MaybeT . return $ evalAexp e store
       ; liftIO $ print (s ++ " " ++ show a)
       ; return store
    }
evalStmt' (S_Assert e s) store =
    do { b <- (MaybeT . return $ evalBexp e store)
       ; if b then
            return store
         else
            MaybeT ((print $ ("Assertion failed: " ++ s))
                >> return Nothing)
       }
