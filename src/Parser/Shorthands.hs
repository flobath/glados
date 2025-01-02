{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Parser.Shorthands where

import Parser.AST

eIf = ExprIfConditional
eCall = ExprFunctionCall
eOp = ExprOperation
eBlk = ExprBlock
eAtom = ExprAtomic

eaInt = ExprAtomic . AtomIntLiteral
eaBool = ExprAtomic . AtomBooleanLiteral
eaId = ExprAtomic . AtomIdentifier

eoAdd a b = ExprOperation $ OpInfix $ InfixAdd a b
eoMul a b = ExprOperation $ OpInfix $ InfixMul a b
