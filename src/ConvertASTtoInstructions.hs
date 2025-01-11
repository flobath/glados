module ConvertASTtoInstructions (
    convertToStackInstructions
) where

import Parser.AST
import StackMachine
import Data.Text (Text, unpack, pack)
import Data.Int (Int64)
import qualified Data.Map as Map


convertToStackInstructions :: Program -> [StackInstruction]
convertToStackInstructions (Program mainFunc functions) =
    let mainInstrs = convertMainFunction mainFunc
        funcInstrs = concatMap convertFunction functions
        allInstrs = mainInstrs ++ funcInstrs
        funcLengths = generateFunctionMap functions
        mainLength = length allInstrs - sum (Map.elems funcLengths)
    in replaceCallFuncName allInstrs funcLengths mainLength

replaceCallFuncName :: [StackInstruction] -> Map.Map String Int -> Int -> [StackInstruction]
replaceCallFuncName instrs funcLengths mainLength = map replace instrs
  where
    funcOffsets = scanl (+) mainLength (Map.elems funcLengths)
    funcMap = Map.fromList $ zip (Map.keys funcLengths) funcOffsets

    replace (CallFuncName name) = case Map.lookup (unpack name) funcMap of
      Just idx -> Call idx
      Nothing -> error $ "Function " ++ unpack name ++ " not found"
    replace instr = instr

generateFunctionMap :: [Function] -> Map.Map String Int
generateFunctionMap functions = Map.fromList $ map getFunctionLength functions
  where
    getFunctionLength (Function name _ _ (BlockExpression stmts)) = (unpack name, length $ concatMap convertStatement stmts)

convertFunction :: Function -> [StackInstruction]
convertFunction (Function _ _ _ (BlockExpression stmts)) = concatMap convertStatement stmts

convertMainFunction :: MainFunction -> [StackInstruction] --TODO: take care of the arguments
convertMainFunction (MainFunction _ (BlockExpression stmts)) = concatMap convertStatement stmts

convertStatement :: Statement -> [StackInstruction]
convertStatement (StExpression expr) = convertExpression expr
convertStatement (StVariableDecl (VariableDeclaration _ (VarIdentifier name)) (Just expr)) =
    convertExpression expr ++ [StoreEnv (unpack name)] -- TODO: take care of the type
convertStatement (StVariableDecl (VariableDeclaration _ (VarIdentifier name)) Nothing) = [PushValue (IntValue 0), StoreEnv (unpack name)] -- TODO: have a default null value and take care of the type
convertStatement (StReturn expr) = convertExpression expr ++ [Return]

convertExpression :: Expression -> [StackInstruction]
convertExpression (ExprAtomic (AtomIntLiteral n)) = [PushValue (IntValue n)]
convertExpression (ExprAtomic (AtomBooleanLiteral b)) = [PushValue (BoolValue b)]
convertExpression (ExprAtomic (AtomIdentifier (VarIdentifier name))) = [PushEnv (unpack name)]

convertExpression (ExprOperation (OpInfix (InfixAdd e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Add]
convertExpression (ExprOperation (OpInfix (InfixSub e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Sub]
convertExpression (ExprOperation (OpInfix (InfixMul e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Mul]
convertExpression (ExprOperation (OpInfix (InfixDiv e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Div]
convertExpression (ExprOperation (OpInfix (InfixMod e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Mod]
convertExpression (ExprOperation (OpInfix (InfixEq e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Eq]
convertExpression (ExprOperation (OpInfix (InfixNeq e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Ne]
convertExpression (ExprOperation (OpInfix (InfixGt e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Gt]
convertExpression (ExprOperation (OpInfix (InfixGe e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Ge]
convertExpression (ExprOperation (OpInfix (InfixLt e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Lt]
convertExpression (ExprOperation (OpInfix (InfixLe e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Le]
convertExpression (ExprOperation (OpInfix (InfixAnd e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue And]
convertExpression (ExprOperation (OpInfix (InfixOr e1 e2))) = convertExpression e1 ++ convertExpression e2 ++ [OpValue Or]

convertExpression (ExprOperation (OpPrefix (PreNot e))) = convertExpression e ++ [PushValue (BoolValue False), OpValue Eq]
convertExpression (ExprOperation (OpPrefix (PrePlus e))) = convertExpression e ++ [PushValue (IntValue 1), OpValue Add]
convertExpression (ExprOperation (OpPrefix (PreNeg e))) = convertExpression e ++ [PushValue (IntValue 1), OpValue Sub]

convertExpression (ExprBlock (BlockExpression stmts)) = concatMap convertStatement stmts

convertExpression (ExprIfConditional cond trueBranch falseBranch) =
    let condInstrs = convertExpression cond
        trueInstrs = convertExpression trueBranch
        falseInstrs = maybe [] convertExpression falseBranch
        jumpFalse = JumpIfFalse (length trueInstrs + 1)
        jumpEnd = Jump (length falseInstrs)
    in condInstrs ++ [jumpFalse] ++ trueInstrs ++ [jumpEnd] ++ falseInstrs

convertExpression (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier name))) args) = [NewEnv] ++ concatMap convertExpression args ++ [CallFuncName name]

convertExpression _ = []


-- TODO: REMOVE THOSES EXAMPLES (move them to test)
-- Example usage
exampleProgram :: Program
exampleProgram = Program mainFunc []

mainFunc :: MainFunction
mainFunc = MainFunction [] (BlockExpression [sumStmt, mulStmt, returnStmt])

sumStmt :: Statement
sumStmt = StExpression (ExprOperation (OpInfix (InfixAdd (ExprAtomic (AtomIntLiteral 5)) (ExprAtomic (AtomIntLiteral 3)))))

mulStmt :: Statement
mulStmt = StExpression (ExprOperation (OpInfix (InfixMul (ExprAtomic (AtomIntLiteral 2)) (ExprAtomic (AtomIntLiteral 4)))))

returnStmt :: Statement
returnStmt = StReturn (ExprAtomic (AtomIntLiteral 0))

-- Example usage of an if statement
ifStmt :: Statement
ifStmt = StExpression (ExprIfConditional
                        (ExprOperation (OpInfix (InfixGt (ExprAtomic (AtomIntLiteral 10)) (ExprAtomic (AtomIntLiteral 5)))))
                        (ExprOperation (OpInfix (InfixAdd (ExprAtomic (AtomIntLiteral 1)) (ExprAtomic (AtomIntLiteral 2)))))
                        (Just (ExprOperation (OpInfix (InfixSub (ExprAtomic (AtomIntLiteral 1)) (ExprAtomic (AtomIntLiteral 2)))))))

exampleProgramWithIf :: Program
exampleProgramWithIf = Program mainFuncWithIf []

mainFuncWithIf :: MainFunction
mainFuncWithIf = MainFunction [] (BlockExpression [ifStmt, returnStmt])

-- Example usage of a function
exampleProgramWithFunction :: Program
exampleProgramWithFunction = Program mainFuncWithFunction [sumFunction]

mainFuncWithFunction :: MainFunction
mainFuncWithFunction = MainFunction [] (BlockExpression [functionCallStmt, returnStmt])

functionCallStmt :: Statement
functionCallStmt = StExpression (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier (pack "sum")))) [])

sumFunction :: Function
sumFunction = Function (pack "sum") [] Nothing (BlockExpression [sumStmt, returnStmt])

--
-- Example usage of multiple functions calling each other
exampleProgramWithMultipleFunctions :: Program
exampleProgramWithMultipleFunctions = Program mainFuncWithMultipleFunctions [funcA, funcB, funcC]

mainFuncWithMultipleFunctions :: MainFunction
mainFuncWithMultipleFunctions = MainFunction [] (BlockExpression [functionCallStmtA, returnStmt])

functionCallStmtA :: Statement
functionCallStmtA = StExpression (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier (pack "funcA")))) [])

funcA :: Function
funcA = Function (pack "funcA") [] Nothing (BlockExpression [functionCallStmtB, returnStmt])

functionCallStmtB :: Statement
functionCallStmtB = StExpression (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier (pack "funcB")))) [])

funcB :: Function
funcB = Function (pack "funcB") [] Nothing (BlockExpression [functionCallStmtC, returnStmt])

functionCallStmtC :: Statement
functionCallStmtC = StExpression (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier (pack "funcC")))) [])

funcC :: Function
funcC = Function (pack "funcC") [] Nothing (BlockExpression [sumStmt, returnStmt])

-- Example usage of a functions map
exampleFunctionMapWithMultipleFunctions :: Map.Map String Int
exampleFunctionMapWithMultipleFunctions = generateFunctionMap [funcA, funcB, funcC]
