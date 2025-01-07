import Parser.AST
import Data.Text (Text, unpack, pack)
import Data.Int (Int64)

--using the AST module, convert the AST to list of instruction to run it to a stack machine

data Value = IntValue Int64 | BoolValue Bool deriving (Show)

data Operator = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or deriving (Show)

data StackInstruction = PushValue Value | PushEnv String | StoreEnv String | Call | Return | Jump Int | JumpIfFalse Int | JumpTo | OpValue Operator deriving (Show)


-- Convert the AST to a list of instructions to run it on a stack machine
convertToStackInstructions :: Program -> [StackInstruction]
convertToStackInstructions (Program mainFunc functions) = convertMainFunction mainFunc

convertMainFunction :: MainFunction -> [StackInstruction]
convertMainFunction (MainFunction _ (BlockExpression stmts)) = concatMap convertStatement stmts

convertStatement :: Statement -> [StackInstruction]
convertStatement (StExpression expr) = convertExpression expr
convertStatement (StVariableDecl (VariableDeclaration _ (VarIdentifier name)) (Just expr)) =
    convertExpression expr ++ [StoreEnv (unpack name)] -- TODO: take care of the type
convertStatement (StVariableDecl (VariableDeclaration _ (VarIdentifier name)) Nothing) = [PushValue (IntValue 0), StoreEnv (unpack name)] -- TODO: have a default null value and take care of the type
convertStatement (StReturn expr) = convertExpression expr ++ [Return]
convertStatement _ = []

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

convertExpression (ExprBlock (BlockExpression stmts)) = concatMap convertStatement stmts

convertExpression (ExprIfConditional cond trueBranch falseBranch) =
    let condInstrs = convertExpression cond
        trueInstrs = convertExpression trueBranch
        falseInstrs = maybe [] convertExpression falseBranch
        jumpFalse = JumpIfFalse (length trueInstrs + 1)
        jumpEnd = Jump (length falseInstrs)
    in condInstrs ++ [jumpFalse] ++ trueInstrs ++ [jumpEnd] ++ falseInstrs

-- Ignore these cases for now
convertExpression (ExprFunctionCall _ _) = []
convertExpression (ExprOperation (OpPrefix _)) = []

convertExpression _ = []

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
