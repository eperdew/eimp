{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Types
( typecheckTStmt
, typecheckTRvalue
, testEqM
, TypeError(..)
, Equal(..)
) where

import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Ast
import Data.Bifunctor

type TypecheckerResult a = Either TypeError a
type TypeMap = Map.Map Identifier Type
type TTypeMap = Map.Map Identifier ATType

lookupT :: TTypeMap -> Identifier -> TypecheckerResult ATType
lookupT ctx id = case Map.lookup id ctx of
    Nothing -> throwError $ UnboundVariable id
    Just t  -> return t

data TypeError
    = MismatchedOperands Type Type
    | InvalidOperands Type Type
    | InvalidOperand Type
    | InvalidAssignment Type Type
    | InvalidCondition
    | Redeclaration Identifier
    | UnboundVariable Identifier
    | UnificationFailure Type Type
    | TypecheckCompilerError String
    deriving (Show)

typecheckTStmt :: TTypeMap -> Stmt () -> TypecheckerResult (TStmt, TTypeMap)
typecheckTStmt ctx (Skip _) = return $ (TSkip, ctx)
typecheckTStmt ctx (Declaration _ id typ) =
    let liftedTyp = liftType typ in
    let ctx' = Map.insert id liftedTyp ctx in
    case Map.lookup id ctx of
        Nothing -> return $ (TSkip, ctx')
        Just _ -> throwError $ Redeclaration id
typecheckTStmt ctx (Assign _ id e) = do
    e ::: t <- typecheckTRvalue ctx e
    ATType t' <- lookupT ctx id
    Eq <- testEqWithError t t' $ InvalidAssignment (lowerTType t) (lowerTType t')
    return (TAssign id (e ::: t), ctx)
typecheckTStmt ctx (Sequence _ stmts) =
    do { (stmts', ctx') <- foldM typecheckTSeq ([], ctx) stmts
       ; return (TSequence $ reverse stmts', ctx') }
typecheckTStmt ctx (If _ e s1 s2) = do
    e ::: t <- typecheckTRvalue ctx e
    (s1,_) <- typecheckTStmt ctx s1
    (s2,_) <- typecheckTStmt ctx s2
    -- Note, we throw away the type information inside the blocks
    -- This prevents us from leaking declarations outside the block
    Eq <- testEqWithError t TBoolean InvalidCondition
    return (TIf e s1 s2, ctx)
typecheckTStmt ctx (While _ e s) = do
    e ::: t <- typecheckTRvalue ctx e
    (s,_) <- typecheckTStmt ctx s
    -- We throw the inner context away - see above
    Eq <- testEqWithError t TBoolean InvalidCondition
    return (TWhile e s, ctx)
typecheckTStmt ctx (Print _ s e) = do
    e ::: t <- typecheckTRvalue ctx e
    Eq <- testEqWithError t TInteger $ InvalidOperand (lowerTType t)
    return (TPrint s e, ctx)
typecheckTStmt ctx (Assert _ e s) = do
    e ::: t <- typecheckTRvalue ctx e
    Eq <- testEqWithError t TBoolean InvalidCondition
    return (TAssert e s, ctx)

typecheckTSeq :: ([TStmt], TTypeMap) -> Stmt () -> TypecheckerResult ([TStmt], TTypeMap)
typecheckTSeq (stmts, ctx) s = do
    (s', ctx') <- typecheckTStmt ctx s
    return (s' : stmts, ctx')

data Equal a b where
    Eq :: Equal a a

testEqM :: TType a -> TType b -> Maybe (Equal a b)
testEqM TInteger TInteger = return Eq
testEqM TBoolean TBoolean = return Eq
testEqM t1 t2 = Nothing

testEq :: TType a -> TType b -> TypecheckerResult (Equal a b)
testEq t1 t2 = case testEqM t1 t2 of
    Nothing -> throwError $ UnificationFailure (lowerTType t1) (lowerTType t2)
    Just witness -> return witness

testEqWithError :: TType a -> TType b -> TypeError -> TypecheckerResult (Equal a b)
testEqWithError a b e = first (const e) $ testEq a b

typecheckTRvalue :: TTypeMap -> Rvalue () -> TypecheckerResult ATRvalue
typecheckTRvalue ctx (Int _ i) = return $ TInt i ::: TInteger
typecheckTRvalue ctx (Boolean _ b) = return $ TBool b ::: TBoolean
typecheckTRvalue ctx (Binop _ op l r) = do
    l ::: tl <- typecheckTRvalue ctx l
    r ::: tr <- typecheckTRvalue ctx r
    ATBinop op top <- return $ typecheckBinopT op
    case top of
        TFun tol (TFun tor to) -> do
            Eq <- testEq tl tol
            Eq <- testEq tr tor
            return $ TBinop op l r ::: to
typecheckTRvalue ctx (Unop _ op e) = do
    e ::: te <- typecheckTRvalue ctx e
    ATUnop op top <- return $ typecheckUnopT op
    case top of
        TFun toperand tresult -> do
            Eq <- testEq te toperand
            return $ TUnop op e ::: tresult
typecheckTRvalue ctx (Var _ id) = case Map.lookup id ctx of
    Just (ATType t) -> return $ TVar id t ::: t
    Nothing -> throwError $ UnboundVariable id

tArithmeticOp :: TType (Integer -> Integer -> Integer)
tArithmeticOp = TFun TInteger (TFun TInteger TInteger)

tBooleanOp :: TType (Bool -> Bool -> Bool)
tBooleanOp = TFun TBoolean (TFun TBoolean TBoolean)

tArithmeticComp :: TType (Integer -> Integer -> Bool)
tArithmeticComp = TFun TInteger (TFun TInteger TBoolean)

typecheckBinopT :: BinaryOperator -> ATBinop
typecheckBinopT Plus      = ATBinop TPlus  tArithmeticOp
typecheckBinopT Minus     = ATBinop TMinus tArithmeticOp
typecheckBinopT Times     = ATBinop TTimes tArithmeticOp
typecheckBinopT Div       = ATBinop TDiv   tArithmeticOp
typecheckBinopT Mod       = ATBinop TMod   tArithmeticOp
typecheckBinopT And       = ATBinop TAnd   tBooleanOp
typecheckBinopT Or        = ATBinop TOr    tBooleanOp
typecheckBinopT Equal     = ATBinop TEqual     tArithmeticComp
typecheckBinopT NotEqual  = ATBinop TNotEqual  tArithmeticComp
typecheckBinopT Less      = ATBinop TLess      tArithmeticComp
typecheckBinopT LessEq    = ATBinop TLessEq    tArithmeticComp
typecheckBinopT Greater   = ATBinop TGreater   tArithmeticComp
typecheckBinopT GreaterEq = ATBinop TGreaterEq tArithmeticComp

typecheckUnopT :: UnaryOperator -> ATUnop
typecheckUnopT UnaryPlus  = ATUnop TUnaryPlus  (TFun TInteger TInteger)
typecheckUnopT UnaryMinus = ATUnop TUnaryMinus (TFun TInteger TInteger)
typecheckUnopT Not        = ATUnop TNot (TFun TBoolean TBoolean)
