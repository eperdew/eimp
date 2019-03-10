module Ast
( Identifier
, Carrier
, extract
, Stmt(..)
, Rvalue(..)
, Type(..)
, BinaryOperator(..)
, UnaryOperator(..)
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map

type Identifier = String

class Carrier c where
    extract :: c a -> a

-- Ast defintions
data Stmt a
    = Skip a
    | Declaration a Identifier Type
    | Assign a Identifier (Rvalue a)
    | Sequence a [Stmt a]
    | If a (Rvalue a) (Stmt a) (Stmt a)
    | While a (Rvalue a) (Stmt a)
    | Print a String (Rvalue a)
    | Assert a (Rvalue a) String
    deriving (Show)

-- TODO: Functor instance for Stmt

instance Carrier Stmt where
    extract (Skip x) = x
    extract (Declaration x _ _) = x
    extract (Assign x _ _) = x
    extract (Sequence x _) = x
    extract (If x _ _ _) = x
    extract (While x _ _) = x
    extract (Print x _ _) = x
    extract (Assert x _ _) = x

data Type
    = IntegerT
    | BooleanT
    | Unit
    deriving (Show, Eq, Ord)

data Rvalue a
    = Int a Integer
    | Boolean a Bool
    | Var a Identifier
    | Binop a BinaryOperator (Rvalue a) (Rvalue a)
    | Unop a UnaryOperator (Rvalue a)
    deriving (Show)

instance Functor Rvalue where
    fmap f (Int x i)          = Int     (f x) i
    fmap f (Boolean x b)      = Boolean (f x) b
    fmap f (Var x id)         = Var     (f x) id
    fmap f (Binop x op e1 e2) = Binop   (f x) op (f <$> e1) (f <$> e2)
    fmap f (Unop x op e)      = Unop    (f x) op (f <$> e)

instance Carrier Rvalue where
    extract (Int     x _)     = x
    extract (Boolean x _)     = x
    extract (Var     x _)     = x
    extract (Binop   x _ _ _) = x
    extract (Unop    x _ _)   = x

data BinaryOperator
    -- Arithmetic operators
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    -- Boolean operators
    | And
    | Or
    -- Comparisons
    | Equal
    | NotEqual
    | Less
    | LessEq
    | Greater
    | GreaterEq
    deriving (Show, Eq)

data UnaryOperator
    -- Arithmetic operators
    = UnaryPlus
    | UnaryMinus
    -- Boolean operators
    | Not
    deriving (Show, Eq)
