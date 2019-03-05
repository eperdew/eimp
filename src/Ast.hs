module Ast
( emptyStore
, updateStore
, eval_aexp
, eval_bexp
, eval_stmt
, eval_stmt'
, Stmt(..)
, Aexp(..)
, Bexp(..)
, A_BinaryOperator(..)
, A_UnaryOperator(..)
, B_BinaryOperator(..)
, B_UnaryOperator(..)
, B_Comparator(..)
) where

type Identifier = String

data Stmt
    = S_Skip
    | S_Assign Identifier Aexp
    | S_Sequence [Stmt]
    | S_If Bexp Stmt Stmt
    | S_While Bexp Stmt
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

type StoreKV k v = k -> Maybe v
type Store = StoreKV String Integer

emptyStore :: StoreKV k v
emptyStore _ = Nothing

updateStore :: (Eq k) => StoreKV k v -> k -> v -> StoreKV k v
updateStore s k v = \key -> if key == k then Just v else s key

eval_aexp :: Aexp -> Store -> Maybe Integer
eval_aexp (A_Int i)          _ = Just i
eval_aexp (A_BinOp op e1 e2) s =
    arithBinopToFunc op
    <$> eval_aexp e1 s
    <*> eval_aexp e2 s
eval_aexp (A_UnaryOp op e)   s =
    arithUnopToFunc  op
    <$> eval_aexp e  s
eval_aexp (A_Var id)         s = s id

eval_bexp :: Bexp -> Store -> Maybe Bool
eval_bexp (B_Boolean b) _ = Just b
eval_bexp (B_BinOp op b1 b2) s =
    boolBinopToFunc op
    <$> eval_bexp b1 s
    <*> eval_bexp b2 s
eval_bexp (B_UnaryOp op b) s =
    boolUnopToFunc op
    <$> eval_bexp b s
eval_bexp (B_Comparison comp a1 a2) s =
    boolComparatorToFunc comp
    <$> eval_aexp a1 s
    <*> eval_aexp a2 s

eval_stmt :: Stmt -> Maybe Store
eval_stmt s = eval_stmt' s emptyStore

eval_stmt' :: Stmt -> Store -> Maybe Store
eval_stmt' S_Skip store = Just store
eval_stmt' (S_Assign id e) store =
    updateStore store id <$> eval_aexp e store
eval_stmt' (S_Sequence stmts) store =
    foldl (\store' stmt -> store' >>= eval_stmt' stmt) (Just store) stmts
eval_stmt' (S_If e s1 s2) store =
    do { b <- eval_bexp e store
       ; if b then eval_stmt' s1 store else eval_stmt' s2 store
    }
eval_stmt' (S_While e s) store =
    eval_stmt' (S_If e (S_Sequence [s, S_While e s]) S_Skip) store
