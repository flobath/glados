module ConvertASTtoInstructions (
    convertToStackInstructions,
) where

import Parser.AST
import StackMachine
import Data.Text (unpack)
import qualified Data.Map as Map


convertToStackInstructions :: Program -> Either String [StackInstruction]
convertToStackInstructions (Program mainFunc functions) = do
    mainInstrs <- Right $ convertMainFunction mainFunc
    funcInstrs <- concat <$> mapM (Right . convertFunction) functions
    let allInstrs = mainInstrs ++ funcInstrs
        funcLengths = generateFunctionMap functions
        mainLength = length allInstrs - sum (Map.elems funcLengths)
    replaceCallFuncName allInstrs funcLengths mainLength

replaceCallFuncName :: [StackInstruction] -> Map.Map String Int -> Int -> Either String [StackInstruction]
replaceCallFuncName instrs funcLengths mainLength = mapM replace instrs
  where
    funcOffsets = scanl (+) mainLength (Map.elems funcLengths)
    funcMap = Map.fromList $ zip (Map.keys funcLengths) funcOffsets

    replace (CallFuncName name) = case Map.lookup (unpack name) funcMap of
      Just idx -> Right (Call idx)
      Nothing -> Left $ "Function " ++ unpack name ++ " not found"
    replace instr = Right instr

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
convertStatement (StAssignment (VarIdentifier name) expr) = convertExpression expr ++ [StoreEnv (unpack name)]
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

convertExpression (ExprWhileLoop cond body) =
    let condInstrs = convertExpression cond
        bodyInstrs = convertExpression body
        jumpBack = Jump (-length bodyInstrs - length condInstrs - 1)
    in condInstrs ++ [JumpIfFalse (length bodyInstrs + 1)] ++ bodyInstrs ++ [jumpBack]

convertExpression (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier name))) args) = [NewEnv] ++ concatMap convertExpression args ++ [CallFuncName name]

convertExpression _ = []
