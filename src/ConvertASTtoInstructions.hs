module ConvertASTtoInstructions (
    convertToStackInstructions
) where

import Parser.AST
import StackMachine
import Data.Text (pack, unpack, Text)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

convertToStackInstructions :: Program -> Either String [StackInstruction]
convertToStackInstructions (Program mainFunc functions) = do
    let declaredVars = Set.empty
    mainInstrs <- convertMainFunction declaredVars functions mainFunc
    funcInstrs <- concat <$> mapM (convertFunction declaredVars functions) functions
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

replaceReturnType :: [StackInstruction] -> Text -> Type -> Either String [StackInstruction]
replaceReturnType instrs funcName expectedType = mapM replace instrs
  where
    replace (ReturnType t)
      | t == expectedType = Right Return
      | t == UnknownType = Right Return
      | otherwise = Left $ "Type mismatch in return for function '" ++ unpack funcName ++ "' expected '" ++ show expectedType ++ "' but got '" ++ show t ++ "'"
    replace instr = Right instr

getFunctionLength :: Set.Set (Text, Type) -> [Function] -> Function -> (Text, Int)
getFunctionLength declaredVars functions func@(Function name _ _ _) =
  case convertFunction declaredVars functions func of
    Right instrs -> (name, length instrs)
    Left err -> (name, 0)

generateFunctionMap :: [Function] -> Map.Map Text Int
generateFunctionMap functions = Map.fromList $ map (getFunctionLength Set.empty functions) functions

isReturnStatement :: Statement -> Bool
isReturnStatement (StReturn _) = True
isReturnStatement _ = False

convertFunction :: Set.Set (Text, Type) -> [Function] -> Function -> Either String [StackInstruction]
convertFunction declaredVars functions (Function funcName params (Just funcType) (BlockExpression stmts)) = do
    let paramNames = Set.fromList $ map (\(VariableDeclaration declaredType (VarIdentifier name)) -> (name, getTypeOfTypeIdentifier declaredType)) params
    let newDeclaredVars = Set.union declaredVars paramNames
    (finalVars, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars functions stmt
        return (newVars, acc ++ stmtInstrs)) (newDeclaredVars, []) stmts
    if null stmts || not (isReturnStatement (last stmts))
        then if getTypeOfTypeIdentifier funcType == VoidType
             then replaceReturnType (instrs ++ [ReturnType VoidType]) funcName VoidType
             else Left $ "Function '" ++ unpack funcName ++ "' must end with a return of type '" ++ show funcType ++ "'"
        else replaceReturnType instrs funcName (getTypeOfTypeIdentifier funcType)
convertFunction declaredVars functions (Function funcName params Nothing (BlockExpression stmts))
    = convertFunction declaredVars functions (Function funcName params (Just (TypeIdentifier (pack "()"))) (BlockExpression stmts))

convertMainFunction :: Set.Set (Text, Type) -> [Function] -> MainFunction -> Either String [StackInstruction]
convertMainFunction declaredVars functions (MainFunction params (BlockExpression stmts)) = do
    let paramNames = Set.fromList $ map (\(VariableDeclaration declaredType (VarIdentifier name)) -> (name, getTypeOfTypeIdentifier declaredType)) params
    let newDeclaredVars = Set.union declaredVars paramNames
    (_, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars functions stmt
        return (newVars, acc ++ stmtInstrs)) (newDeclaredVars, []) stmts
    if null stmts || not (isReturnStatement (last stmts))
        then Left "Main function must end with a return of type 'i32'"
        else replaceReturnType instrs (pack "main") IntType

convertStatement :: Set.Set (Text, Type) -> [Function] -> Statement -> Either String (Set.Set (Text, Type), [StackInstruction])
convertStatement declaredVars functions (StExpression expr) = do
    exprInstrs <- convertExpression declaredVars functions expr
    return (declaredVars, exprInstrs)
convertStatement declaredVars functions (StVariableDecl (VariableDeclaration declaredType@(TypeIdentifier delaredTypeName) (VarIdentifier name)) (Just expr)) = do
    let exprType = getTypeOfExpression declaredVars functions expr
    if exprType == getTypeOfTypeIdentifier declaredType
        then do
            exprInstrs <- convertExpression declaredVars functions expr
            let newDeclaredVars = Set.insert (name, getTypeOfTypeIdentifier declaredType) declaredVars
            return (newDeclaredVars, exprInstrs ++ [StoreEnv name])
        else Left $ "Type mismatch in variable declaration for '" ++ unpack name ++ "'" ++ " expected '" ++ unpack delaredTypeName ++ "' but got '" ++ show exprType ++ "'"
convertStatement declaredVars _ (StVariableDecl (VariableDeclaration declaredType (VarIdentifier name)) Nothing) = do
    let newDeclaredVars = Set.insert (name, getTypeOfTypeIdentifier declaredType) declaredVars
    return (newDeclaredVars, [PushValue (IntValue 0), StoreEnv name])
convertStatement declaredVars functions (StAssignment (VarIdentifier name) expr) =
    case findVariableType declaredVars name of
        Just varType -> do
            let exprType = getTypeOfExpression declaredVars functions expr
            if varType == exprType
                then do
                    exprInstrs <- convertExpression declaredVars functions expr
                    return (declaredVars, exprInstrs ++ [StoreEnv name])
                else Left $ "Type mismatch in assignment for '" ++ unpack name ++ "'" ++ " expected '" ++ show varType ++ "' but got '" ++ show exprType ++ "'"
        Nothing -> Left $ "Variable '" ++ unpack name ++ "' not declared"
convertStatement declaredVars functions (StReturn expr) = do
    exprInstrs <- convertExpression declaredVars functions expr
    return (declaredVars, exprInstrs ++ [ReturnType (getTypeOfExpression declaredVars functions expr)])

convertExpression :: Set.Set (Text, Type) -> [Function] -> Expression -> Either String [StackInstruction]
convertExpression declaredVars functions (ExprAtomic (AtomIdentifier (VarIdentifier name))) =
    if Set.member (name, getTypeOfVariable declaredVars name) declaredVars
        then Right [PushEnv name]
        else Left $ "Variable '" ++ unpack name ++ "' not declared"
convertExpression _ _ (ExprAtomic (AtomIntLiteral n)) = Right [PushValue (IntValue n)]
convertExpression _ _ (ExprAtomic (AtomBooleanLiteral b)) = Right [PushValue (BoolValue b)]

convertExpression declaredVars functions (ExprOperation (OpInfix (InfixAdd e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Add
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixSub e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Sub
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixMul e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Mul
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixDiv e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Div
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixMod e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Mod
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixEq e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Eq
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixNeq e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Ne
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixGt e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Gt
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixGe e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Ge
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixLt e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Lt
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixLe e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Le
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixAnd e1 e2))) = convertInfixOperation declaredVars functions e2 e1 And
convertExpression declaredVars functions (ExprOperation (OpInfix (InfixOr e1 e2))) = convertInfixOperation declaredVars functions e2 e1 Or

convertExpression declaredVars functions (ExprOperation (OpPrefix (PreNot e))) = do
    eInstrs <- convertExpression declaredVars functions e
    return $ eInstrs ++ [PushValue (BoolValue False), OpValue Eq]
convertExpression declaredVars functions (ExprOperation (OpPrefix (PrePlus e))) = do
    eInstrs <- convertExpression declaredVars functions e
    return $ eInstrs
convertExpression declaredVars functions (ExprOperation (OpPrefix (PreNeg e))) = do
    eInstrs <- convertExpression declaredVars functions e
    return $ eInstrs ++ [PushValue (IntValue -1), OpValue Mul]

convertExpression declaredVars functions (ExprBlock (BlockExpression stmts)) = do
    (_, instrs) <- foldM (\(vars, acc) stmt -> do
        (newVars, stmtInstrs) <- convertStatement vars functions stmt
        return (newVars, acc ++ stmtInstrs)) (declaredVars, []) stmts
    return instrs

convertExpression declaredVars functions (ExprIfConditional cond trueBranch falseBranch) = do
    condInstrs <- convertExpression declaredVars functions cond
    trueInstrs <- convertExpression declaredVars functions trueBranch
    falseInstrs <- maybe (Right []) (convertExpression declaredVars functions) falseBranch
    let jumpFalse = JumpIfFalse (length trueInstrs + 2)
        jumpEnd = Jump (length falseInstrs + 1)
    return $ condInstrs ++ [jumpFalse] ++ trueInstrs ++ [jumpEnd] ++ falseInstrs

convertExpression declaredVars functions (ExprWhileLoop cond body) = do
    condInstrs <- convertExpression declaredVars functions cond
    bodyInstrs <- convertExpression declaredVars functions body
    let jumpFalse = JumpIfFalse (length bodyInstrs + 2)
        jumpBack = Jump (-length bodyInstrs - length condInstrs - 1)
    return $ condInstrs ++ [jumpFalse] ++ bodyInstrs ++ [jumpBack]

convertExpression declaredVars functions (ExprDoWhileLoop body cond) = do
    bodyInstrs <- convertExpression declaredVars functions body
    condInstrs <- convertExpression declaredVars functions cond
    let jumpFalse = JumpIfFalse 2
        jumpBack = Jump (-length bodyInstrs - length condInstrs - 1)
    return $ bodyInstrs ++ condInstrs ++ [jumpFalse] ++ [jumpBack]

convertExpression declaredVars functions (ExprForLoop block) = do
    convertExpression declaredVars functions $ ExprBlock block

convertExpression declaredVars functions (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier name))) args) = do
    argsInstrs <- concat <$> mapM (convertExpression declaredVars functions) args
    let argTypes = map (getTypeOfExpression declaredVars functions) args
    return $ argsInstrs ++ [NewEnv] ++ zipWith (\i argType -> StoreArgs name argType (length args - i - 1)) [0..] argTypes ++ [CallFuncName name]

convertExpression _ _ _ = Right []

convertInfixOperation :: Set.Set (Text, Type) -> [Function] -> Expression -> Expression -> Operator -> Either String [StackInstruction]
convertInfixOperation declaredVars functions e1 e2 op = do
    e1Instrs <- convertExpression declaredVars functions e1
    e2Instrs <- convertExpression declaredVars functions e2
    return $ e1Instrs ++ e2Instrs ++ [OpValue op]

--

findVariableType :: Set.Set (Text, Type) -> Text -> Maybe Type
findVariableType declaredVars name =
    let filteredVars = Set.filter (\(varName, _) -> varName == name) declaredVars
        in if Set.null filteredVars then Nothing else Just (snd $ Set.elemAt 0 filteredVars)

getTypeOfVariable :: Set.Set (Text, Type) -> Text -> Type
getTypeOfVariable declaredVars name = fromMaybe UnknownType (findVariableType declaredVars name)

getTypeOfTypeIdentifier :: TypeIdentifier -> Type
getTypeOfTypeIdentifier (TypeIdentifier typename) = case unpack typename of
    "i32" -> IntType
    "bool" -> BoolType
    "()" -> VoidType
    _ -> UnknownType

getTypeOfExpression :: Set.Set (Text, Type) -> [Function] -> Expression -> Type
getTypeOfExpression _ _ (ExprAtomic (AtomIntLiteral _)) = IntType
getTypeOfExpression _ _ (ExprAtomic (AtomBooleanLiteral _)) = BoolType
getTypeOfExpression declaredVars _ (ExprAtomic (AtomIdentifier (VarIdentifier name))) = do
    fromMaybe UnknownType (findVariableType declaredVars name)
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixAdd _ _))) = IntType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixSub _ _))) = IntType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixMul _ _))) = IntType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixDiv _ _))) = IntType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixMod _ _))) = IntType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixEq _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixNeq _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixGt _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixGe _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixLt _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixLe _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixAnd _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpInfix (InfixOr _ _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpPrefix (PreNot _))) = BoolType
getTypeOfExpression _ _ (ExprOperation (OpPrefix (PrePlus _))) = IntType
getTypeOfExpression _ _ (ExprOperation (OpPrefix (PreNeg _))) = IntType
getTypeOfExpression _ _ (ExprBlock _) = UnknownType
getTypeOfExpression declaredVars functions (ExprIfConditional _ trueBranch (Just falseBranch)) =
    let trueType = getTypeOfExpression declaredVars functions trueBranch
        falseType = getTypeOfExpression declaredVars functions falseBranch
    in if trueType == falseType then trueType else UnknownType
getTypeOfExpression declaredVars functions (ExprIfConditional _ trueBranch Nothing) =
    getTypeOfExpression declaredVars functions trueBranch
getTypeOfExpression _ _ (ExprWhileLoop _ _) = UnknownType
getTypeOfExpression _ _ (ExprDoWhileLoop _ _) = UnknownType
getTypeOfExpression _ functions (ExprFunctionCall (ExprAtomic (AtomIdentifier (VarIdentifier name))) _) =
    maybe
        UnknownType getTypeOfTypeIdentifier
        (lookup
            name [(name', type_) | Function name' _ (Just type_) _ <- functions])

getTypeOfExpression _ _ _ = UnknownType

