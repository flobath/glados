{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module CompilerSpec (spec) where

import Test.Hspec
import Parser.AST
import StackMachine
import ConvertASTtoInstructions

import Data.Text (pack)
import Parser.Shorthands

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
            Function (pack "f") [VariableDeclaration (typeId "i32") $ varId "x", VariableDeclaration (typeId "i32") $ varId "y"] (Just $ typeId "i32")
                (BlockExpression [StReturn $ sumExpr (varRef "x") (ExprFunctionCall (varRef "inc") [varRef "y"])]),
            Function (pack "inc") [VariableDeclaration (typeId "i32") $ varId "a"] (Just $ typeId "i32")
                (BlockExpression [StReturn $ sumExpr (varRef "a") (intConstant 1)])
        ]
--Program with function that have no return Type
programG = Program (MainFunction [] (BlockExpression [localIntDecl "a" 5, localIntDecl "b" 5,
        StExpression $ ExprFunctionCall (varRef "f") [varRef "a", varRef "b"], StReturn $ intConstant 0
    ])) [
            Function (pack "f") [VariableDeclaration (typeId "i32") $ varId "x", VariableDeclaration (typeId "i32") $ varId "y"] Nothing
                (BlockExpression [StExpression $ sumExpr (varRef "x") (ExprFunctionCall (varRef "inc") [varRef "y"])]),
            Function (pack "inc") [VariableDeclaration (typeId "i32") $ varId "a"] (Just $ typeId "i32")
                (BlockExpression [StReturn $ sumExpr (varRef "a") (intConstant 1)])
        ]

errorProgramA = Program (MainFunction [] (BlockExpression [StReturn $ ExprFunctionCall (varRef "f") []])) []
errorProgramB = Program (MainFunction [] (BlockExpression [StReturn $ intConstant 0])) [Function (pack "my_add") [VariableDeclaration (typeId "i32") (varId "a"), VariableDeclaration (typeId "i32") (varId "b")] (Just $ typeId "i32") (BlockExpression [StVariableDecl (VariableDeclaration (typeId "i32") (varId "result")) (Just (intConstant 1)), StReturn (sumExpr (varRef "a") (varRef "z"))])]
errorProgramC = Program (MainFunction [] (BlockExpression [StVariableDecl (VariableDeclaration (typeId "i32") $ varId "a") $ Just $ boolConstant True, StReturn $ intConstant 0])) []
errorProgramD = Program (MainFunction [] (BlockExpression [StVariableDecl (VariableDeclaration (typeId "i32") $ varId "a") $ Just $ intConstant 5, StAssignment (varId "a") (boolConstant True), StReturn $ intConstant 0])) []
errorProgramE = Program (MainFunction [] (BlockExpression [StVariableDecl (VariableDeclaration (typeId "i32") $ varId "a") $ Just $ intConstant 5, StAssignment (varId "a") $ ExprFunctionCall (varRef "f") [varRef "a"], StReturn $ intConstant 0])) [Function (pack "f") [VariableDeclaration (typeId "bool") $ varId "x"] (Just $ typeId "i32") (BlockExpression [StReturn $ intConstant 5])]
errorProgramF = Program (MainFunction [] (BlockExpression [StVariableDecl (VariableDeclaration (typeId "i32") $ varId "a") $ Just $ intConstant 5, StAssignment (varId "a") $ ExprFunctionCall (varRef "f") [varRef "a"], StReturn $ intConstant 0])) [Function (pack "f") [VariableDeclaration (typeId "i32") $ varId "x"] (Just $ typeId "bool") (BlockExpression [StReturn $ boolConstant False])]
errorProgramG = Program (MainFunction [] (BlockExpression [StVariableDecl (VariableDeclaration (typeId "i32") $ varId "a") $ Just $ intConstant 5, StAssignment (varId "a") $ ExprFunctionCall (varRef "f") [varRef "a"], StReturn $ intConstant 0])) [Function (pack "f") [VariableDeclaration (typeId "i32") $ varId "x"] (Just $ typeId "i32") (BlockExpression [StReturn $ boolConstant False])]
errorProgramH = Program (MainFunction [] (BlockExpression [StExpression $ intConstant 0])) []
errorProgramI = Program (MainFunction [] (BlockExpression [localIntDecl "a" 5, localIntDecl "b" 5,
          StExpression $ ExprFunctionCall (varRef "f") [varRef "a", varRef "b"], StReturn $ intConstant 0
      ])) [
            Function (pack "f") [VariableDeclaration (typeId "i32") $ varId "x", VariableDeclaration (typeId "i32") $ varId "y"] Nothing
                (BlockExpression [StReturn $ sumExpr (varRef "x") (ExprFunctionCall (varRef "inc") [varRef "y"])]),
            Function (pack "inc") [VariableDeclaration (typeId "i32") $ varId "a"] (Just $ typeId "i32")
                (BlockExpression [StReturn $ sumExpr (varRef "a") (intConstant 1)])
          ]

spec :: Spec
spec = do
    describe "Full programs" $ do
        it "Simple addition return" $ do
            convertToStackInstructions programA `shouldBe` Right [PushValue (IntValue 5), PushValue (IntValue 3), OpValue Add, Return]
        it "Simple variable return" $ do
            convertToStackInstructions programB `shouldBe` Right [PushValue (IntValue 5), StoreEnv "a", PushEnv "a", Return]
        it "Simple variable addition return" $ do
            convertToStackInstructions programC `shouldBe` Right [PushValue (IntValue 5), StoreEnv "a", PushEnv "a", PushValue (IntValue 5), OpValue Add, Return]
        it "Simple conditional return" $ do
            convertToStackInstructions programD `shouldBe` Right [PushValue (IntValue 5), StoreEnv "a", PushEnv "a", PushValue (IntValue 10), OpValue Gt, JumpIfFalse 3, PushValue (IntValue 0), Jump 2, PushValue (IntValue 1), Return]
        it "All operators" $ do
            convertToStackInstructions programE `shouldBe` Right [
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 5),
                    StoreEnv "b",
                    PushEnv "a",
                    PushEnv "b",
                    OpValue Add,
                    StoreEnv "b",
                    PushValue (IntValue 5),
                    PushEnv "b",
                    OpValue Eq,
                    JumpIfFalse 3,
                    PushValue (IntValue 1),
                    Jump 1,
                    StoreEnv "a",
                    PushValue (IntValue 10),
                    PushEnv "a",
                    OpValue Mul,
                    StoreEnv "a",
                    PushValue (IntValue 2),
                    PushEnv "a",
                    OpValue Sub,
                    StoreEnv "a",
                    PushValue (IntValue 0),
                    PushEnv "a",
                    OpValue Ge,
                    JumpIfFalse 3,
                    PushValue (IntValue 1),
                    Jump 2,
                    PushValue (IntValue 0),
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    PushValue (IntValue 2),
                    PushEnv "a",
                    OpValue Mod,
                    StoreEnv "a",
                    PushValue (BoolValue False),
                    StoreEnv "c",
                    PushValue (IntValue 0),
                    PushEnv "a",
                    OpValue Ne,
                    PushEnv "c",
                    PushValue (BoolValue False),
                    OpValue Eq,
                    OpValue And,
                    JumpIfFalse 3,
                    PushValue (IntValue 1),
                    Jump 2,
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
                PushEnv "a",
                PushEnv "b",
                NewEnv,
                StoreEnv "y",
                StoreEnv "x",
                Call 11,
                Return,
                PushEnv "y",
                NewEnv,
                StoreEnv "a",
                Call 18,
                PushEnv "x",
                OpValue Add,
                Return,
                PushValue (IntValue 1),
                PushEnv "a",
                OpValue Add,
                Return
              ]
        it "Function call without return type" $ do
            convertToStackInstructions programG `shouldBe` Right [
                PushValue (IntValue 5),
                StoreEnv "a",
                PushValue (IntValue 5),
                StoreEnv "b",
                PushEnv "a",
                PushEnv "b",
                NewEnv,
                StoreEnv "y",
                StoreEnv "x",
                Call 12,
                PushValue (IntValue 0),
                Return,
                PushEnv "y",
                NewEnv,
                StoreEnv "a",
                Call 19,
                PushEnv "x",
                OpValue Add,
                Return,
                PushValue (IntValue 1),
                PushEnv "a",
                OpValue Add,
                Return
              ]
        it "Missing function" $ do
            convertToStackInstructions errorProgramA `shouldBe` Left "Function 'f' not defined"
        it "Missing variable" $ do
            convertToStackInstructions errorProgramB `shouldBe` Left "Variable 'z' not declared"
        it "Invalid type declaration" $ do
            convertToStackInstructions errorProgramC `shouldBe` Left "Type mismatch in variable declaration for 'a' expected 'i32' but got 'bool'"
        it "Invalide type assignment" $ do
            convertToStackInstructions errorProgramD `shouldBe` Left "Type mismatch in assignment for 'a' expected 'i32' but got 'bool'"
        it "Invalid type function arguments" $ do
            convertToStackInstructions errorProgramE `shouldBe` Left "Type mismatch for argument 1 in function 'f' expected 'bool' but got 'i32'"
        it "Invalid type function call" $ do
            convertToStackInstructions errorProgramF `shouldBe` Left "Type mismatch in assignment for 'a' expected 'i32' but got 'bool'"
        it "Invalid return type function definition" $ do
            convertToStackInstructions errorProgramG `shouldBe` Left "Type mismatch in return for function 'f' expected 'i32' but got 'bool'"
        it "No return main function" $ do
            convertToStackInstructions errorProgramH `shouldBe` Left "Main function must end with a return of type 'i32'"
        it "Function should not return" $ do
            convertToStackInstructions errorProgramI `shouldBe` Left "Type mismatch in return for function 'f' expected '()' but got 'i32'"
        it "Simple while return" $ do
            convertToStackInstructions (Program (fnMain [] [
                        sDecl (tId "i32") (vId "a") (Just $ eaInt 5),
                        sExpr $ eWhile
                            (eoLt (eaId "a") (eaInt 7))
                            (eBlk [
                                sAssi "a" (eoAdd (eaId "a") (eaInt 1))
                            ]),
                        sRet $ eaId "a"
                    ]
                ) [])
            `shouldBe` Right [
                    PushValue (IntValue 5),
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    JumpIfFalse 6,
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    Jump (-8),
                    PushEnv "a",
                    Return
                ]
        it "Simple until return" $ do
            convertToStackInstructions (Program (fnMain [] [
                        sDecl (tId "i32") (vId "a") (Just $ eaInt 10),
                        sExpr $ eWhile
                            (eoNot $ eoLt (eaId "a") (eaInt 7))
                            (eBlk [
                                sAssi "a" (eoSub (eaId "a") (eaInt 1))
                            ]),
                        sRet $ eaId "a"
                ]) [])
            `shouldBe` Right [
                    PushValue (IntValue 10),
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    PushValue (BoolValue False),
                    OpValue Eq,
                    JumpIfFalse 6,
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Sub,
                    StoreEnv "a",
                    Jump (-10),
                    PushEnv "a",
                    Return
                ]
        it "Simple do while return" $ do
            convertToStackInstructions (Program (fnMain [] [
                        sDecl (tId "i32") (vId "a") (Just $ eaInt 5),
                        sExpr $ eDoWhile
                            (eBlk [
                                sAssi "a" (eoAdd (eaId "a") (eaInt 1))
                            ])
                            (eoLt (eaId "a") (eaInt 7)),
                        sRet $ eaId "a"
                    ]
                ) [])
            `shouldBe` Right [
                    PushValue (IntValue 5),
                    StoreEnv "a",
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    JumpIfFalse 2,
                    Jump (-8),
                    PushEnv "a",
                    Return
                ]
        it "Simple do until return" $ do
            convertToStackInstructions (Program (fnMain [] [
                        sDecl (tId "i32") (vId "a") (Just $ eaInt 10),
                        sExpr $ eDoWhile
                            (eBlk [
                                sAssi "a" (eoSub (eaId "a") (eaInt 1))
                            ])
                            (eoNot $ eoLt (eaId "a") (eaInt 7)),
                        sRet $ eaId "a"
                ]) [])
            `shouldBe` Right [
                    PushValue (IntValue 10),
                    StoreEnv "a",
                    PushValue (IntValue 1),
                    PushEnv "a",
                    OpValue Sub,
                    StoreEnv "a",
                    PushValue (IntValue 7),
                    PushEnv "a",
                    OpValue Lt,
                    PushValue (BoolValue False),
                    OpValue Eq,
                    JumpIfFalse 2,
                    Jump (-10),
                    PushEnv "a",
                    Return
                ]
        it "Simple for return" $ do
            convertToStackInstructions (Program (fnMain [] [
                    sDecl (tId "i32") (vId "a") (Just $ eaInt 0),
                    sExpr $ eFor [
                            sDecl (tId "i32") (vId "i") (Just $ eaInt 0),
                            sExpr $ eWhile
                                (eoLt (eaId "i") (eaInt 5))
                                (eBlk [
                                    sAssi "a" (eoAdd (eaId "a") (eaId "i")),
                                    sAssi "i" (eoAdd (eaId "i") (eaInt 1))
                                ])
                        ],
                    sRet $ eaId "a"
                ]) [])
            `shouldBe` Right [
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 0),
                    StoreEnv "i",
                    PushValue (IntValue 5),
                    PushEnv "i",
                    OpValue Lt,
                    JumpIfFalse 10,
                    PushEnv "i",
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    PushValue (IntValue 1),
                    PushEnv "i",
                    OpValue Add,
                    StoreEnv "i",
                    Jump (-12),
                    PushEnv "a",
                    Return
                ]
        it "Simple for return with reverse range" $ do
            convertToStackInstructions (Program (fnMain [] [
                    sDecl (tId "i32") (vId "a") (Just $ eaInt 0),
                    sExpr $ eFor [
                            sDecl (tId "i32") (vId "i") (Just $ eaInt 5),
                            sExpr $ eWhile
                                (eoGt (eaId "i") (eaInt 0))
                                (eBlk [
                                    sAssi "a" (eoAdd (eaId "a") (eaId "i")),
                                    sAssi "i" (eoAdd (eaId "i") (eaInt (-1)))
                                ])
                    ],
                    sRet $ eaId "a"
                ]) [])
            `shouldBe` Right [
                    PushValue (IntValue 0),
                    StoreEnv "a",
                    PushValue (IntValue 5),
                    StoreEnv "i",
                    PushValue (IntValue 0),
                    PushEnv "i",
                    OpValue Gt,
                    JumpIfFalse 10,
                    PushEnv "i",
                    PushEnv "a",
                    OpValue Add,
                    StoreEnv "a",
                    PushValue (IntValue (-1)),
                    PushEnv "i",
                    OpValue Add,
                    StoreEnv "i",
                    Jump (-12),
                    PushEnv "a",
                    Return
                ]
