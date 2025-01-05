module Execute (execute) where
import StackMachine (Value(..), Operator(..), Instruction(..), Stack, Program)

add = infixIntOperation "add" (+)
sub = infixIntOperation "sub" (-)
mul = infixIntOperation "mul" (*)
division = infixIntOperation "div" div
modulo = infixIntOperation "mod" mod

eq :: Stack -> Bool
eq [BoolValue x] = x
eq [IntValue _] = True
eq (BoolValue x:BoolValue y:_) = y == x
eq (IntValue x:IntValue y:_) = y == x
eq _ = False

neq :: Stack -> Bool
neq [BoolValue x] = not x
neq [IntValue _] = False
neq (BoolValue x:BoolValue y:_) = y /= x
neq (IntValue x:IntValue y:_) = y /= x
neq _ = False

lt :: Stack -> Bool
lt (IntValue x:IntValue y:_) = y < x
lt _ = False

le :: Stack -> Bool
le [] = False
le [_] = False
le (IntValue x:IntValue y:_) = y <= x
le _ = False

gt :: Stack -> Bool
gt [] = False
gt [_] = False
gt (IntValue x:IntValue y:_) = y > x
gt _ = False

ge :: Stack -> Bool
ge [] = False
ge [_] = False
ge [IntValue x, IntValue y] = y >= x
ge (IntValue x:IntValue y:_) = y >= x
ge _ = False

push :: Stack -> Value -> Stack
push xs x = x:xs

-- pushArgs :: Stack -> Stack -> Stack
-- pushArgs xs ys = ys ++ xs

-- pushEnv :: Stack -> Stack -> Stack
-- pushEnv xs ys = ys ++ xs

infixIntOperation :: String -> (Int -> Int -> Int) -> (Stack -> Stack)
infixIntOperation _ op (IntValue x1 : IntValue x2 : xs) = (IntValue $ x2 `op` x1):xs
infixIntOperation name _ _ = error $ "Invalid arguments for " ++ name

applyOperator :: Operator -> Stack -> Stack
applyOperator Add list = infixIntOperation "add" (+) list
applyOperator Sub list = infixIntOperation "sub" (-) list
applyOperator Mul list = infixIntOperation "mul" (*) list
applyOperator Div list = infixIntOperation "div" div list
applyOperator Mod list = infixIntOperation "mod" mod list
applyOperator _ _ = error "Unsupported operator"


call :: Stack -> Stack
call (OpValue op:xs) = applyOperator op xs
call _ = error "Cannot call"

jumpIfTrue :: Stack -> Bool
jumpIfTrue (OpValue Eq:xs) = eq xs
jumpIfTrue (OpValue Ne:xs) = neq xs
jumpIfTrue (OpValue Lt:xs) = lt xs
jumpIfTrue (OpValue Le:xs) = le xs
jumpIfTrue (OpValue Gt:xs) = gt xs
jumpIfTrue (OpValue Ge:xs) = ge xs
jumpIfTrue _ = False

jumpIfFalse :: Stack -> Bool
jumpIfFalse (OpValue Eq:xs) = neq xs
jumpIfFalse (OpValue Ne:xs) = eq xs
jumpIfFalse (OpValue Lt:xs) = ge xs
jumpIfFalse (OpValue Le:xs) = gt xs
jumpIfFalse (OpValue Gt:xs) = le xs
jumpIfFalse (OpValue Ge:xs) = lt xs
jumpIfFalse _ = False

jump :: Bool -> Int -> Program -> Int -> Int
jump condition offset instructions currentIndex
  | condition = let targetIndex = currentIndex + offset
                in if targetIndex < 0 || targetIndex >= length instructions
                     then error "Jump out of bounds"
                     else targetIndex
  | otherwise = currentIndex + 1  -- Proceed to the next instruction


execute :: Program -> Stack -> Int -> Int
execute instructions stack currentIndex
  | currentIndex >= length instructions = error "Program terminated unexpectedly"
  | currentIndex < 0 = error "Invalid program counter"
  | otherwise =
      case instructions !! currentIndex of
        Return ->
          case stack of
            (IntValue x:_) -> x
            _ -> error "Invalid return value"
        Push x ->
          execute instructions (push stack x) (currentIndex + 1)
        -- PushArgs n ->
        --   execute instructions (pushArgs stack n) (currentIndex + 1)
        -- PushEnv x ->
        --   execute instructions (pushEnv stack x) (currentIndex + 1)
        Call ->
          execute instructions (call stack) (currentIndex + 1)
        JumpIfTrue offset ->
          let condition = jumpIfTrue stack
          in execute instructions stack (jump condition offset instructions currentIndex)
        JumpIfFalse offset ->
          let condition = jumpIfFalse stack
          in execute instructions stack (jump condition offset instructions currentIndex)
        _ -> error "Unsupported instruction"
