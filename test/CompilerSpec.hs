{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CompilerSpec (spec) where

import Test.Hspec
import Parser.AST
import StackMachine
import ConvertASTtoInstructions

import Data.Text (pack)

typeId x = TypeIdentifier (pack x)
varId x = VarIdentifier (pack x)
varRef x = ExprAtomic (AtomIdentifier (VarIdentifier (pack x)))

intConstant x = ExprAtomic (AtomIntLiteral x)
boolConstant x = ExprAtomic (AtomBooleanLiteral x)
sumExpr a b = ExprOperation (OpInfix (InfixAdd a b))
subExpr a b = ExprOperation (OpInfix (InfixSub a b))
mulExpr a b = ExprOperation (OpInfix (InfixMul a b))
modExpr a b = ExprOperation (OpInfix (InfixMod a b))

setVar x = StAssignment (VarIdentifier (pack x))
localDecl name = StVariableDecl (VariableDeclaration (typeId "i32") $ varId name) Nothing
localIntDecl name x = StVariableDecl (VariableDeclaration (typeId "i32") $ varId name) $ Just $ intConstant x
localBoolDecl name x = StVariableDecl (VariableDeclaration (typeId "bool") $ varId name) $ Just $ boolConstant x

programA = Program (MainFunction [] (BlockExpression [StReturn $ sumExpr (intConstant 3) (intConstant 5)])) []
programB = Program (MainFunction [] (BlockExpression [localIntDecl "a" 5, StReturn (varRef "a")])) []
programC = Program (MainFunction [] (BlockExpression [localIntDecl "a" 5, StReturn (ExprOperation (OpInfix (InfixAdd (intConstant 5) (varRef "a"))))])) []
programD = Program (MainFunction [] (BlockExpression [localIntDecl "a" 5, StReturn (ExprIfConditional (ExprOperation (OpInfix (InfixGt (intConstant 10) (varRef "a")))) (intConstant 0) $ Just (intConstant 1))])) []

spec :: Spec
spec = do
    describe "Full programs" $ do
        it "Simple addition return" $ do
            convertToStackInstructions programA `shouldBe` Right [PushValue (IntValue 3), PushValue (IntValue 5), OpValue Add, Return]
        it "Simple variable return" $ do
            convertToStackInstructions programB `shouldBe` Right [PushValue (IntValue 5), StoreEnv "a", PushEnv "a", Return]
        it "Simple variable addition return" $ do
            convertToStackInstructions programC `shouldBe` Right [PushValue (IntValue 5), StoreEnv "a", PushValue (IntValue 5), PushEnv "a", OpValue Add, Return]
        it "Simple conditional return" $ do
            convertToStackInstructions programD `shouldBe` Right [PushValue (IntValue 5), StoreEnv "a", PushValue (IntValue 10), PushEnv "a", OpValue Gt, JumpIfFalse 2, PushValue (IntValue 0), Jump 1, PushValue (IntValue 1), Return]
