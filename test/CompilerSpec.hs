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
programE = Program (MainFunction [] (BlockExpression [localDecl "a", setVar "a" $ intConstant 0,
        StExpression $ ExprBlock $ BlockExpression [
                localIntDecl "b" 5,
                setVar "b" $ sumExpr (varRef "b") (varRef "a"),
                setVar "a" $ ExprIfConditional (ExprOperation (OpInfix (InfixEq (varRef "b") (intConstant 5)))) (intConstant 1) Nothing
            ],
        setVar "a" $ mulExpr (varRef "a") (intConstant 10),
        setVar "a" $ subExpr (varRef "a") (intConstant 2),
        setVar "a" $ sumExpr (varRef "a") $ ExprIfConditional (ExprOperation (OpInfix (InfixGe (varRef "a") (intConstant 0)))) (intConstant 1) (Just (intConstant 0)),
        setVar "a" $ modExpr (varRef "a") (intConstant 2),
        localBoolDecl "c" False,
        setVar "a" $ ExprIfConditional (ExprOperation (OpInfix (InfixAnd (ExprOperation (OpPrefix (PreNot (varRef "c")))) (ExprOperation (OpInfix (InfixNeq (varRef "a") (intConstant 0))))))) (intConstant 1) (Just (intConstant 0)),
        StReturn $ varRef "a"
    ])) []
programF = Program (MainFunction [] (BlockExpression [localIntDecl "a" 5, localIntDecl "b" 5,
        StReturn $ ExprFunctionCall (varRef "f") [varRef "a", varRef "b"]
    ])) [
            Function (pack "f") [VariableDeclaration (typeId "i32") $ varId "a", VariableDeclaration (typeId "i32") $ varId "b"] (Just $ typeId "i32")
                (BlockExpression [StReturn $ sumExpr (varRef "a") (ExprFunctionCall (varRef "inc") [varRef "b"])]),
            Function (pack "inc") [VariableDeclaration (typeId "i32") $ varId "a"] (Just $ typeId "i32")
                (BlockExpression [StReturn $ sumExpr (varRef "a") (intConstant 1)])
        ]
programG = Program (MainFunction [] (BlockExpression [StReturn $ ExprFunctionCall (varRef "f") []])) []
programH = Program (MainFunction [] (
        BlockExpression [
            localIntDecl "a" 5,
            StExpression $ ExprWhileLoop
                (ExprOperation (OpInfix (InfixLt (varRef "a") (intConstant 7))))
                (ExprBlock (BlockExpression [setVar "a" (sumExpr (varRef "a") (intConstant 1))])),
            StReturn (varRef "a")
        ]
    )) []

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
        it "All operators" $ do
            convertToStackInstructions programE `shouldBe` Right [
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 5),
                    StoreEnv "b",
                    PushEnv "b",
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "b",
                    PushEnv "b",
                    PushValue (IntValue 5),
                    OpValue Eq,
                    JumpIfFalse 2,
                    PushValue (IntValue 1),
                    Jump 0,
                    StoreEnv "a",
                    PushEnv "a",
                    PushValue (IntValue 10),
                    OpValue Mul,
                    StoreEnv "a",
                    PushEnv "a",
                    PushValue (IntValue 2),
                    OpValue Sub,
                    StoreEnv "a",
                    PushEnv "a",
                    PushEnv "a",
                    PushValue (IntValue 0),
                    OpValue Ge,
                    JumpIfFalse 2,
                    PushValue (IntValue 1),
                    Jump 1,
                    PushValue (IntValue 0),
                    OpValue Add,
                    StoreEnv "a",
                    PushEnv "a",
                    PushValue (IntValue 2),
                    OpValue Mod,
                    StoreEnv "a",
                    PushValue (BoolValue False),
                    StoreEnv "c",
                    PushEnv "c",
                    PushValue (BoolValue False),
                    OpValue Eq,
                    PushEnv "a",
                    PushValue (IntValue 0),
                    OpValue Ne,
                    OpValue And,
                    JumpIfFalse 2,
                    PushValue (IntValue 1),
                    Jump 1,
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushEnv "a",
                    Return
                ]
        it "Function call" $ do
            convertToStackInstructions programF `shouldBe` Right [
                    PushValue (IntValue 5),
                    StoreEnv "a",
                    PushValue (IntValue 5),
                    StoreEnv "b",
                    NewEnv,
                    PushEnv "a",
                    PushEnv "b",
                    Call 9,
                    Return,
                    PushEnv "a",
                    NewEnv,
                    PushEnv "b",
                    Call 15,
                    OpValue Add,
                    Return,
                    PushEnv "a",
                    PushValue (IntValue 1),
                    OpValue Add,
                    Return
                ]
        it "Missing function" $ do
            convertToStackInstructions programG `shouldBe` Left "Function f not found"
        it "Simple while return" $ do
            convertToStackInstructions programH `shouldBe` Right [
                    PushValue (IntValue 5),
                    StoreEnv "a",
                    PushEnv "a",
                    PushValue (IntValue 7),
                    OpValue Lt,
                    JumpIfFalse 6,
                    PushEnv "a",
                    PushValue (IntValue 1),
                    OpValue Add,
                    StoreEnv "a",
                    Jump (-8),
                    PushEnv "a",
                    Return
                ]
