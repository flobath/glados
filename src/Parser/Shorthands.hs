{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser.Shorthands where

import Parser.AST

eIf = ExprIfConditional
eCall = ExprFunctionCall
eOp = ExprOperation
eBlk = ExprBlock
eAtom = ExprAtomic

eaInt = ExprAtomic . AtomIntLiteral
eaBool = ExprAtomic . AtomBooleanLiteral
eaId = ExprAtomic . AtomIdentifier . VarIdentifier

eoAdd a b = ExprOperation $ OpInfix $ InfixAdd a b
eoSub a b = ExprOperation $ OpInfix $ InfixSub a b
eoMul a b = ExprOperation $ OpInfix $ InfixMul a b
eoDiv a b = ExprOperation $ OpInfix $ InfixDiv a b
eoMod a b = ExprOperation $ OpInfix $ InfixMod a b
eoEq a b = ExprOperation $ OpInfix $ InfixEq a b
eoNeq a b = ExprOperation $ OpInfix $ InfixNeq a b
eoGt a b = ExprOperation $ OpInfix $ InfixGt a b
eoLt a b = ExprOperation $ OpInfix $ InfixLt a b
eoGe a b = ExprOperation $ OpInfix $ InfixGe a b
eoLe a b = ExprOperation $ OpInfix $ InfixLe a b
