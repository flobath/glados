module ConvertASTtoInstructions (
    convertToStackInstructions
) where

import Parser.AST
import StackMachine
import Data.Text (unpack)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace (trace)

convertToStackInstructions :: Program -> Either String [StackInstruction]
convertToStackInstructions (Program mainFunc functions) = do
    let declaredVars = Set.empty
    mainInstrs <- convertMainFunction declaredVars mainFunc
    funcInstrs <- concat <$> mapM (convertFunction declaredVars) functions
    let allInstrs = mainInstrs ++ funcInstrs
        funcLengths = generateFunctionMap functions
        mainLength = length allInstrs - sum (Map.elems funcLengths)
    instrsWithCallFuncName <- replaceCallFuncName allInstrs funcLengths mainLength
    replaceStoreArgsWithEnv instrsWithCallFuncName functions

replaceStoreArgsWithEnv :: [StackInstruction] -> [Function] -> Either String [StackInstruction]
replaceStoreArgsWithEnv instrs functions = mapM replace instrs
  where
    funcMap = Map.fromList [(unpack name, params) | Function name params _ _ <- functions]

    replace (StoreArgs funcName idx) = case Map.lookup (unpack funcName) funcMap of
      Just params -> if idx < length params
                     then let (VariableDeclaration _ (VarIdentifier paramName)) = params !! idx
                          in Right (StoreEnv (unpack paramName))
                     else Left $ "Index " ++ show idx ++ " out of bounds for function '" ++ unpack funcName ++ "'"
      Nothing -> Left $ "Function '" ++ unpack funcName ++ "' not defined"
    replace instr = Right instr

replaceCallFuncName :: [StackInstruction] -> Map.Map String Int -> Int -> Either String [StackInstruction]
replaceCallFuncName instrs funcLengths mainLength = mapM replace instrs
  where
    funcOffsets = scanl (+) mainLength (Map.elems funcLengths)
    funcMap = Map.fromList $ zip (Map.keys funcLengths) funcOffsets

    replace (CallFuncName name) = case Map.lookup (unpack name) funcMap of
      Just idx -> Right (Call idx)
      Nothing -> Left $ "Function '" ++ unpack name ++ "' not defined"
    replace instr = Right instr

getFunctionLength :: Set.Set String -> Function -> (String, Int)
getFunctionLength declaredVars func@(Function name _ _ _) =
  case convertFunction declaredVars func of
    Right instrs -> (unpack name, length instrs)
    Left err -> (unpack name, 0)

generateFunctionMap :: [Function] -> Map.Map String Int
generateFunctionMap functions = Map.fromList $ map (getFunctionLength Set.empty) functions

convertFunction :: Set.Set String -> Function -> Either String [StackInstruction]
convertFunction declaredVars (Function _ params _ (BlockExpression stmts)) = do
    let paramNames = Set.fromList $ map (\(VariableDeclaration _ (VarIdentifier name)) -> unpack name) params
    let newDeclaredVars = Set.union declaredVars paramNames
    (finalVars, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars stmt
        return (newVars, acc ++ stmtInstrs)) (newDeclaredVars, []) stmts
    return instrs

convertMainFunction :: Set.Set String -> MainFunction -> Either String [StackInstruction]
convertMainFunction declaredVars (MainFunction params (BlockExpression stmts)) = do
    let paramNames = Set.fromList $ map (\(VariableDeclaration _ (VarIdentifier name)) -> unpack name) params
    let newDeclaredVars = Set.union declaredVars paramNames
    (_, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars stmt
        return (newVars, acc ++ stmtInstrs)) (newDeclaredVars, []) stmts
    return instrs

convertStatement :: Set.Set String -> Statement -> Either String (Set.Set String, [StackInstruction])
convertStatement declaredVars (StExpression expr) = do
    exprInstrs <- convertExpression declaredVars expr
    return (declaredVars, exprInstrs)
convertStatement declaredVars (StVariableDecl (VariableDeclaration _ (VarIdentifier name)) (Just expr)) = do
    exprInstrs <- convertExpression declaredVars expr
    let newDeclaredVars = Set.insert (unpack name) declaredVars
    return (newDeclaredVars, exprInstrs ++ [StoreEnv (unpack name)])
convertStatement declaredVars (StVariableDecl (VariableDeclaration _ (VarIdentifier name)) Nothing) = do
    let newDeclaredVars = Set.insert (unpack name) declaredVars
    return (newDeclaredVars, [PushValue (IntValue 0), StoreEnv (unpack name)])
convertStatement declaredVars (StAssignment (VarIdentifier name) expr) = do
    if Set.member (unpack name) declaredVars
        then do
            exprInstrs <- convertExpression declaredVars expr
            return (declaredVars, exprInstrs ++ [StoreEnv (unpack name)])
        else Left $ "Variable '" ++ unpack name ++ "' not declared"
convertStatement declaredVars (StReturn expr) = do
    exprInstrs <- convertExpression declaredVars expr
    return (declaredVars, exprInstrs ++ [Return])

convertExpression :: Set.Set String -> Expression -> Either String [StackInstruction]
convertExpression declaredVars (ExprAtomic (AtomIdentifier (VarIdentifier name))) =
    if Set.member (unpack name) declaredVars
        then Right [PushEnv (unpack name)]
        else Left $ "Variable '" ++ unpack name ++ "' not declared"
convertExpression declaredVars (ExprAtomic (AtomIntLiteral n)) = Right [PushValue (IntValue n)]
convertExpression declaredVars (ExprAtomic (AtomBooleanLiteral b)) = Right [PushValue (BoolValue b)]

convertExpression declaredVars (ExprOperation (OpInfix (InfixAdd e1 e2))) = convertInfixOperation declaredVars e2 e1 Add
convertExpression declaredVars (ExprOperation (OpInfix (InfixSub e1 e2))) = convertInfixOperation declaredVars e2 e1 Sub
convertExpression declaredVars (ExprOperation (OpInfix (InfixMul e1 e2))) = convertInfixOperation declaredVars e2 e1 Mul
convertExpression declaredVars (ExprOperation (OpInfix (InfixDiv e1 e2))) = convertInfixOperation declaredVars e2 e1 Div
convertExpression declaredVars (ExprOperation (OpInfix (InfixMod e1 e2))) = convertInfixOperation declaredVars e2 e1 Mod
convertExpression declaredVars (ExprOperation (OpInfix (InfixEq e1 e2))) = convertInfixOperation declaredVars e2 e1 Eq
convertExpression declaredVars (ExprOperation (OpInfix (InfixNeq e1 e2))) = convertInfixOperation declaredVars e2 e1 Ne
convertExpression declaredVars (ExprOperation (OpInfix (InfixGt e1 e2))) = convertInfixOperation declaredVars e2 e1 Gt
convertExpression declaredVars (ExprOperation (OpInfix (InfixGe e1 e2))) = convertInfixOperation declaredVars e2 e1 Ge
convertExpression declaredVars (ExprOperation (OpInfix (InfixLt e1 e2))) = convertInfixOperation declaredVars e2 e1 Lt
convertExpression declaredVars (ExprOperation (OpInfix (InfixLe e1 e2))) = convertInfixOperation declaredVars e2 e1 Le
convertExpression declaredVars (ExprOperation (OpInfix (InfixAnd e1 e2))) = convertInfixOperation declaredVars e2 e1 And
convertExpression declaredVars (ExprOperation (OpInfix (InfixOr e1 e2))) = convertInfixOperation declaredVars e2 e1 Or

convertExpression declaredVars (ExprOperation (OpPrefix (PreNot e))) = do
    eInstrs <- convertExpression declaredVars e
    return $ eInstrs ++ [PushValue (BoolValue False), OpValue Eq]
convertExpression declaredVars (ExprOperation (OpPrefix (PrePlus e))) = do
    eInstrs <- convertExpression declaredVars e
    return $ eInstrs ++ [PushValue (IntValue 1), OpValue Add]
convertExpression declaredVars (ExprOperation (OpPrefix (PreNeg e))) = do
    eInstrs <- convertExpression declaredVars e
    return $ eInstrs ++ [PushValue (IntValue 1), OpValue Sub]

convertExpression declaredVars (ExprBlock (BlockExpression stmts)) = do
    (_, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars stmt
        return (newVars, acc ++ stmtInstrs)) (declaredVars, []) stmts
    return instrs

convertExpression declaredVars (ExprIfConditional cond trueBranch falseBranch) = do
    condInstrs <- convertExpression declaredVars cond
    trueInstrs <- convertExpression declaredVars trueBranch
    falseInstrs <- maybe (Right []) (convertExpression declaredVars) falseBranch
    let jumpFalse = JumpIfFalse (length trueInstrs + 2)
        jumpEnd = Jump (length falseInstrs + 1)
    return $ condInstrs ++ [jumpFalse] ++ trueInstrs ++ [jumpEnd] ++ falseInstrs

convertExpression declaredVars (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier name))) args) = do
    argsInstrs <- concat <$> mapM (convertExpression declaredVars) args
    return $ [NewEnv] ++ argsInstrs ++ zipWith (\i _ -> StoreArgs name ((length args) - i - 1)) [0..] args ++ [CallFuncName name]

convertExpression _ _ = Right []

convertInfixOperation :: Set.Set String -> Expression -> Expression -> Operator -> Either String [StackInstruction]
convertInfixOperation declaredVars e1 e2 op = do
    e1Instrs <- convertExpression declaredVars e1
    e2Instrs <- convertExpression declaredVars e2
    return $ e1Instrs ++ e2Instrs ++ [OpValue op]

