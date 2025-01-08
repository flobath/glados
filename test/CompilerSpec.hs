module CompilerSpec (spec) where

import Test.Hspec
import Parser.AST
import StackMachine
import ConvertASTtoInstructions

import qualified Data.Map as Map
import Data.Text (pack)
import Parser.AST (MainFunction(MainFunction), BlockExpression (BlockExpression), Statement (StReturn, StVariableDecl), VariableDeclaration (VariableDeclaration), Expression (ExprAtomic), AtomicExpression (AtomIdentifier))
import StackMachine (StackInstruction(StoreEnv))

sumExpr = ExprOperation (OpInfix (InfixAdd (ExprAtomic (AtomIntLiteral 5)) (ExprAtomic (AtomIntLiteral 3))))

i32Type = TypeIdentifier (pack "i32")
varA = VarIdentifier (pack "a")

localIntStmt x = StVariableDecl (VariableDeclaration i32Type varA) $ Just $ ExprAtomic (AtomIntLiteral x)

programA = Program (MainFunction [] (BlockExpression [StReturn sumExpr])) []
programB = Program (MainFunction [] (BlockExpression [localIntStmt 5, StReturn (ExprAtomic (AtomIdentifier varA))])) []

spec :: Spec
spec = do
    describe "Full programs" $ do
        it "Simple addition return" $ do
            convertToStackInstructions programA `shouldBe` [PushValue (IntValue 5), PushValue (IntValue 3), OpValue Add, Return]
        it "Simple variable return" $ do
            convertToStackInstructions programB `shouldBe` [PushValue (IntValue 5), StoreEnv "a", PushEnv "a", Return]
