{-# LANGUAGE GADTs, ExistentialQuantification, StandaloneDeriving #-}

module Ast
( Identifier
, Carrier
, extract
, lowerTType
, liftType
, Stmt(..)
, Rvalue(..)
, TOperator(..)
, TRvalue(..)
, ATRvalue(..)
, ATBinop(..)
, ATUnop(..)
, TStmt(..)
, TType(..)
, ATType(..)
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

data TStmt
    = TSkip
    | TAssign Identifier ATRvalue
    | TSequence [TStmt]
    | TIf (TRvalue Bool) TStmt TStmt
    | TWhile (TRvalue Bool) TStmt
    | TPrint String (TRvalue Integer)
    | TAssert (TRvalue Bool) String
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
    | Fun Type Type
    | Unit
    deriving (Show, Eq, Ord)

data TType a where
    TInteger :: TType Integer
    TBoolean :: TType Bool
    TFun :: TType a -> TType b -> TType (a -> b)

deriving instance Show (TType a)

data ATType = forall a. ATType (TType a)

deriving instance Show ATType

liftType :: Type -> ATType
liftType IntegerT = ATType TInteger
liftType BooleanT = ATType TBoolean
liftType (Fun a b) = case (liftType a, liftType b) of
    (ATType ta, ATType tb) -> ATType $ TFun ta tb

lowerTType :: TType a -> Type
lowerTType TInteger = IntegerT
lowerTType TBoolean = BooleanT
lowerTType (TFun a b) = Fun (lowerTType a) (lowerTType b)

data Rvalue a
    = Int a Integer
    | Boolean a Bool
    | Var a Identifier
    | Binop a BinaryOperator (Rvalue a) (Rvalue a)
    | Unop a UnaryOperator (Rvalue a)
    deriving (Show)

data TRvalue t where
    TInt   :: Integer -> TRvalue Integer
    TBool  :: Bool -> TRvalue Bool
    TVar   :: Identifier -> TType a -> TRvalue a
    TBinop :: TOperator (a -> b -> c) -> TRvalue a -> TRvalue b -> TRvalue c
    TUnop  :: TOperator (a -> b) -> TRvalue a -> TRvalue b
deriving instance Show (TRvalue t)

data ATRvalue = forall a. TRvalue a ::: TType a
deriving instance Show (ATRvalue)

data TOperator a where
    TPlus  :: TOperator (Integer -> Integer -> Integer)
    TMinus :: TOperator (Integer -> Integer -> Integer)
    TTimes :: TOperator (Integer -> Integer -> Integer)
    TDiv   :: TOperator (Integer -> Integer -> Integer)
    TMod   :: TOperator (Integer -> Integer -> Integer)
    -- Boolean operators
    TAnd :: TOperator (Bool -> Bool -> Bool)
    TOr  :: TOperator (Bool -> Bool -> Bool)
    -- Comparisons
    TEqual    :: TOperator (Integer -> Integer -> Bool)
    TNotEqual :: TOperator (Integer -> Integer -> Bool)
    TLess      :: TOperator (Integer -> Integer -> Bool)
    TLessEq    :: TOperator (Integer -> Integer -> Bool)
    TGreater   :: TOperator (Integer -> Integer -> Bool)
    TGreaterEq :: TOperator (Integer -> Integer -> Bool)
    -- Unary operators
    TUnaryPlus  :: TOperator (Integer -> Integer)
    TUnaryMinus :: TOperator (Integer -> Integer)
    -- Boolean operators
    TNot :: TOperator (Bool -> Bool)
deriving instance Show (TOperator a)

data ATBinop = forall a b c. (Eq a, Eq b, Eq c) =>
    ATBinop (TOperator (a -> b -> c)) (TType (a -> b -> c))
data ATUnop = forall a b. (Eq a, Eq b) =>
    ATUnop (TOperator (a -> b)) (TType (a -> b))

deriving instance Show ATBinop
deriving instance Show ATUnop

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
