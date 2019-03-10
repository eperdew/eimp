module Types
( typecheckStmt
, typecheckRvalue
, TypeError(..)
) where

import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Ast

type TypecheckerResult a = Either TypeError a
type TypeMap = Map.Map Identifier Type

data TypeError
    = MismatchedOperands Type Type
    | InvalidOperands Type Type
    | InvalidOperand Type
    | InvalidAssignment Type Type
    | InvalidCondition
    | Redeclaration Identifier
    | UnboundVariable Identifier
    deriving (Show)

typecheckStmt :: TypeMap -> Stmt () -> TypecheckerResult (Stmt Type, TypeMap)
typecheckStmt ctx (Skip _) = return $ (Skip Unit, ctx)
typecheckStmt ctx (Declaration _ id typ) =
    let ctx' = Map.insert id typ ctx in
    case Map.lookup id ctx of
        Nothing -> return $ (Declaration typ id typ, ctx')
        Just _ -> throwError $ Redeclaration id
typecheckStmt ctx (Assign _ id e) =
    do { e' <- typecheckRvalue ctx e
       ; let t = extract e' in
         case Map.lookup id ctx of
             Nothing -> throwError $ UnboundVariable id
             Just t' -> if t /= t' then throwError $ InvalidAssignment t t'
                        else return $ (Assign Unit id e', ctx) }
typecheckStmt ctx (Sequence _ stmts) =
    do { (stmts', ctx') <- foldM typecheckSeq ([], ctx) stmts
       ; return (Sequence Unit $ reverse stmts', ctx') }
typecheckStmt ctx (If _ e s1 s2) =
    do { e'      <- typecheckRvalue ctx e
       ; (s1',_) <- typecheckStmt ctx s1
       ; (s2',_) <- typecheckStmt ctx s2
       ; if extract e' /= BooleanT
         then throwError $ InvalidCondition
         -- Note, we throw away the type information inside the blocks
         -- This prevents us from leaking declarations outside the block
         else return $ (If Unit e' s1' s2', ctx) }
typecheckStmt ctx (While _ e s) =
    do { e' <- typecheckRvalue ctx e
       ; (s',_) <- typecheckStmt ctx s
       ; if extract e' /= BooleanT
         then throwError $ InvalidCondition
         -- We throw the inner context away - see above
         else return $ (While Unit e' s', ctx) }
typecheckStmt ctx (Print _ s e) =
    do { e' <- typecheckRvalue ctx e
       ; return $ (Print Unit s e', ctx) }
typecheckStmt ctx (Assert _ e s) =
    do { e' <- typecheckRvalue ctx e
       ; if extract e' /= BooleanT
         then throwError $ InvalidCondition
         else return $ (Assert Unit e' s, ctx) }

typecheckSeq :: ([Stmt Type], TypeMap) -> Stmt () -> TypecheckerResult ([Stmt Type], TypeMap)
typecheckSeq (stmts, ctx) s =
    do { (s', ctx') <- typecheckStmt ctx s
       ; return $ (s' : stmts, ctx')
    }

typecheckRvalue :: TypeMap -> Rvalue () -> TypecheckerResult (Rvalue Type)
typecheckRvalue ctx (Int _ i) = return $ Int IntegerT i
typecheckRvalue ctx (Boolean _ b) = return $ Boolean BooleanT b
typecheckRvalue ctx (Var _ id) =
    case Map.lookup id ctx of
        Nothing -> throwError $ UnboundVariable id
        Just typ -> return $ Var typ id
typecheckRvalue ctx (Binop _ op e1 e2) =
    do { e1' <- typecheckRvalue ctx e1
       ; e2' <- typecheckRvalue ctx e2
       ; typ <- typecheckBinop op (extract e1') (extract e2')
       ; return $ Binop typ op e1' e2'
    }
typecheckRvalue ctx (Unop _ op e) =
    do { e' <- typecheckRvalue ctx e
       ; typ <- typecheckUnop op (extract e')
       ; return $ Unop typ op e' }

typecheckBinop :: BinaryOperator -> Type -> Type -> TypecheckerResult Type
typecheckBinop Plus  IntegerT IntegerT = return IntegerT
typecheckBinop Minus IntegerT IntegerT = return IntegerT
typecheckBinop Times IntegerT IntegerT = return IntegerT
typecheckBinop Div   IntegerT IntegerT = return IntegerT
typecheckBinop Mod   IntegerT IntegerT = return IntegerT
typecheckBinop And BooleanT BooleanT = return BooleanT
typecheckBinop Or  BooleanT BooleanT = return BooleanT
typecheckBinop Equal    t1 t2 = if t1 == t2
                                then return BooleanT
                                else throwError $ MismatchedOperands t1 t2
typecheckBinop NotEqual t1 t2 = if t1 == t2
                                then return BooleanT
                                else throwError $ MismatchedOperands t1 t2
typecheckBinop Less      IntegerT IntegerT = return BooleanT
typecheckBinop LessEq    IntegerT IntegerT = return BooleanT
typecheckBinop Greater   IntegerT IntegerT = return BooleanT
typecheckBinop GreaterEq IntegerT IntegerT = return BooleanT
typecheckBinop _ t1 t2 = throwError $ InvalidOperands t1 t2
    -- Make this type error more precise

typecheckUnop :: UnaryOperator -> Type -> TypecheckerResult Type
typecheckUnop UnaryPlus  IntegerT = return IntegerT
typecheckUnop UnaryMinus IntegerT = return IntegerT
typecheckUnop Not BooleanT = return BooleanT
typecheckUnop _ t = throwError $ InvalidOperand t
    -- Make this type error more precise
