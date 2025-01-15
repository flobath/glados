{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser.Shorthands where

import Parser.AST

tId = TypeIdentifier
vId = VarIdentifier
vdecl t1 t2 = VariableDeclaration (TypeIdentifier t1) (VarIdentifier t2)
eIf = ExprIfConditional
eWhile = ExprWhileLoop
eDoWhile = ExprDoWhileLoop
eFor = ExprForLoop
eCall = ExprFunctionCall
eOp = ExprOperation
eBlk = ExprBlock . BlockExpression
eAtom = ExprAtomic

eaInt = ExprAtomic . AtomIntLiteral
eaBool = ExprAtomic . AtomBooleanLiteral
eaId = ExprAtomic . AtomIdentifier . VarIdentifier

eoNeg = ExprOperation . OpPrefix . PreNeg
eoPlus = ExprOperation . OpPrefix . PrePlus
eoNot = ExprOperation . OpPrefix . PreNot
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
eoOr a b = ExprOperation $ OpInfix $ InfixOr a b
eoAnd a b = ExprOperation $ OpInfix $ InfixAnd a b

sRet = StReturn
sDecl t n = StVariableDecl (VariableDeclaration t n)
sExpr = StExpression
sAssi t = StAssignment (VarIdentifier t)

fn text params retType sts = Function text params retType (BlockExpression sts)
fnMain params sts = MainFunction params (BlockExpression sts)
