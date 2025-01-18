module ConvertASTtoInstructions (
    convertToStackInstructions
) where

import Parser.AST
import StackMachine
import Data.Text (unpack, Text)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set


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
    funcMap = Map.fromList [(name, params) | Function name params _ _ <- functions]

    replace (StoreArgs funcName argType idx) = case Map.lookup funcName funcMap of
      Just params -> if idx < length params
                     then let (VariableDeclaration declaredType (VarIdentifier paramName)) = params !! idx
                              declaredType' = getTypeOfTypeIdentifier declaredType
                          in if argType == declaredType'
                             then Right (StoreEnv paramName)
                             else Left $ "Type mismatch for argument " ++ show (idx + 1) ++ " in function '" ++ unpack funcName ++ "' expected '" ++ show declaredType' ++ "' but got '" ++ show argType ++ "'"
                     else Left $ "Index " ++ show idx ++ " out of bounds for function '" ++ unpack funcName ++ "'"
      Nothing -> Left $ "Function '" ++ unpack funcName ++ "' not defined"
    replace instr = Right instr

replaceCallFuncName :: [StackInstruction] -> Map.Map Text Int -> Int -> Either String [StackInstruction]
replaceCallFuncName instrs funcLengths mainLength = mapM replace instrs
  where
    funcOffsets = scanl (+) mainLength (Map.elems funcLengths)
    funcMap = Map.fromList $ zip (Map.keys funcLengths) funcOffsets

    replace (CallFuncName name) = case Map.lookup name funcMap of
      Just idx -> Right (Call idx)
      Nothing -> Left $ "Function '" ++ unpack name ++ "' not defined"
    replace instr = Right instr

getFunctionLength :: Set.Set (Text, Type) -> Function -> (Text, Int)
getFunctionLength declaredVars func@(Function name _ _ _) =
  case convertFunction declaredVars func of
    Right instrs -> (name, length instrs)
    Left err -> (name, 0)

generateFunctionMap :: [Function] -> Map.Map Text Int
generateFunctionMap functions = Map.fromList $ map (getFunctionLength Set.empty) functions

convertFunction :: Set.Set (Text, Type) -> Function -> Either String [StackInstruction]
convertFunction declaredVars (Function _ params _ (BlockExpression stmts)) = do
    let paramNames = Set.fromList $ map (\(VariableDeclaration declaredType (VarIdentifier name)) -> (name, getTypeOfTypeIdentifier declaredType)) params
    let newDeclaredVars = Set.union declaredVars paramNames
    (finalVars, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars stmt
        return (newVars, acc ++ stmtInstrs)) (newDeclaredVars, []) stmts
    return instrs

convertMainFunction :: Set.Set (Text, Type) -> MainFunction -> Either String [StackInstruction]
convertMainFunction declaredVars (MainFunction params (BlockExpression stmts)) = do
    let paramNames = Set.fromList $ map (\(VariableDeclaration declaredType (VarIdentifier name)) -> (name, getTypeOfTypeIdentifier declaredType)) params
    let newDeclaredVars = Set.union declaredVars paramNames
    (_, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars stmt
        return (newVars, acc ++ stmtInstrs)) (newDeclaredVars, []) stmts
    return instrs

convertStatement :: Set.Set (Text, Type) -> Statement -> Either String (Set.Set (Text, Type), [StackInstruction])
convertStatement declaredVars (StExpression expr) = do
    exprInstrs <- convertExpression declaredVars expr
    return (declaredVars, exprInstrs)
convertStatement declaredVars (StVariableDecl (VariableDeclaration declaredType@(TypeIdentifier delaredTypeName) (VarIdentifier name)) (Just expr)) = do
    let exprType = getTypeOfExpression declaredVars expr
    if exprType == getTypeOfTypeIdentifier declaredType || exprType == ToBeDefType
        then do
            exprInstrs <- convertExpression declaredVars expr
            let newDeclaredVars = Set.insert (name, getTypeOfTypeIdentifier declaredType) declaredVars
            return (newDeclaredVars, exprInstrs ++ [StoreEnv name])
        else Left $ "Type mismatch in variable declaration for '" ++ unpack name ++ "'" ++ " expected '" ++ unpack delaredTypeName ++ "' but got '" ++ show exprType ++ "'"
convertStatement declaredVars (StVariableDecl (VariableDeclaration declaredType (VarIdentifier name)) Nothing) = do
    let newDeclaredVars = Set.insert (name, getTypeOfTypeIdentifier declaredType) declaredVars
    return (newDeclaredVars, [PushValue (IntValue 0), StoreEnv name])
convertStatement declaredVars (StAssignment (VarIdentifier name) expr) =
    case findVariableType declaredVars name of
        Just varType -> do
            let exprType = getTypeOfExpression declaredVars expr
            if varType == exprType || exprType == ToBeDefType
                then do
                    exprInstrs <- convertExpression declaredVars expr
                    return (declaredVars, exprInstrs ++ [StoreEnv name])
                else Left $ "Type mismatch in assignment for '" ++ unpack name ++ "'" ++ " expected '" ++ show varType ++ "' but got '" ++ show exprType ++ "'"
        Nothing -> Left $ "Variable '" ++ unpack name ++ "' not declared"
convertStatement declaredVars (StReturn expr) = do
    exprInstrs <- convertExpression declaredVars expr
    return (declaredVars, exprInstrs ++ [Return])

convertExpression :: Set.Set (Text, Type) -> Expression -> Either String [StackInstruction]
convertExpression declaredVars (ExprAtomic (AtomIdentifier (VarIdentifier name))) =
    if Set.member (name, getTypeOfVariable declaredVars name) declaredVars
        then Right [PushEnv name]
        else Left $ "Variable '" ++ unpack name ++ "' not declared"
convertExpression _ (ExprAtomic (AtomIntLiteral n)) = Right [PushValue (IntValue n)]
convertExpression _ (ExprAtomic (AtomBooleanLiteral b)) = Right [PushValue (BoolValue b)]

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

convertExpression declaredVars (ExprWhileLoop cond body) = do
    condInstrs <- convertExpression declaredVars cond
    bodyInstrs <- convertExpression declaredVars body
    let jumpFalse = JumpIfFalse (length bodyInstrs + 2)
        jumpBack = Jump (-length bodyInstrs - length condInstrs - 1)
    return $ condInstrs ++ [jumpFalse] ++ bodyInstrs ++ [jumpBack]

convertExpression declaredVars (ExprDoWhileLoop body cond) = do
    bodyInstrs <- convertExpression declaredVars body
    condInstrs <- convertExpression declaredVars cond
    let jumpFalse = JumpIfFalse 2
        jumpBack = Jump (-length bodyInstrs - length condInstrs - 1)
    return $ bodyInstrs ++ condInstrs ++ [jumpFalse] ++ [jumpBack]

convertExpression declaredVars (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier name))) args) = do
    argsInstrs <- concat <$> mapM (convertExpression declaredVars) args
    let argTypes = map (getTypeOfExpression declaredVars) args
    return $ [NewEnv] ++ argsInstrs ++ zipWith (\i argType -> StoreArgs name argType (length args - i - 1)) [0..] argTypes ++ [CallFuncName name]

convertExpression _ _ = Right []

convertInfixOperation :: Set.Set (Text, Type) -> Expression -> Expression -> Operator -> Either String [StackInstruction]
convertInfixOperation declaredVars e1 e2 op = do
    e1Instrs <- convertExpression declaredVars e1
    e2Instrs <- convertExpression declaredVars e2
    return $ e1Instrs ++ e2Instrs ++ [OpValue op]

--

findVariableType :: Set.Set (Text, Type) -> Text -> Maybe Type
findVariableType declaredVars name =
    let filteredVars = Set.filter (\(varName, _) -> varName == name) declaredVars
        in if Set.null(filteredVars) then Nothing else Just (snd $ Set.elemAt 0 filteredVars)

getTypeOfVariable :: Set.Set (Text, Type) -> Text -> Type
getTypeOfVariable declaredVars name =
    case findVariableType declaredVars name of
        Just t -> t
        Nothing -> UnknownType

getTypeOfTypeIdentifier :: TypeIdentifier -> Type
getTypeOfTypeIdentifier (TypeIdentifier typename) = case unpack typename of
    "i32" -> IntType
    "bool" -> BoolType
    _ -> UnknownType

getTypeOfExpression :: Set.Set (Text, Type) -> Expression -> Type
getTypeOfExpression _ (ExprAtomic (AtomIntLiteral _)) = IntType
getTypeOfExpression _ (ExprAtomic (AtomBooleanLiteral _)) = BoolType
getTypeOfExpression declaredVars (ExprAtomic (AtomIdentifier (VarIdentifier name))) = do
    case findVariableType declaredVars name of
        Just t -> t
        Nothing -> UnknownType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixAdd _ _))) = IntType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixSub _ _))) = IntType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixMul _ _))) = IntType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixDiv _ _))) = IntType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixMod _ _))) = IntType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixEq _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixNeq _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixGt _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixGe _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixLt _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixLe _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixAnd _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpInfix (InfixOr _ _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpPrefix (PreNot _))) = BoolType
getTypeOfExpression _ (ExprOperation (OpPrefix (PrePlus _))) = IntType
getTypeOfExpression _ (ExprOperation (OpPrefix (PreNeg _))) = IntType
getTypeOfExpression _ (ExprBlock _) = UnknownType
getTypeOfExpression declaredVars (ExprIfConditional _ trueBranch (Just falseBranch)) =
    let trueType = getTypeOfExpression declaredVars trueBranch
        falseType = getTypeOfExpression declaredVars falseBranch
    in if trueType == falseType then trueType else UnknownType
getTypeOfExpression declaredVars (ExprIfConditional _ trueBranch Nothing) =
    getTypeOfExpression declaredVars trueBranch
getTypeOfExpression _ (ExprWhileLoop _ _) = UnknownType
getTypeOfExpression _ (ExprDoWhileLoop _ _) = UnknownType
getTypeOfExpression _ (ExprFunctionCall _ _) = ToBeDefType
getTypeOfExpression _ _ = UnknownType

