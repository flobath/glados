module Execute (execute) where
import StackMachine (Value(..), Operator(..), Instruction(..))

add :: [Value] -> [Value]
add [] = error "Invalid arguments for add"
add [_] = error "Invalid arguments for add"
add [IntValue x, IntValue y] = [IntValue (y + x)]
add (IntValue x:IntValue y:xs) = [IntValue (y + x)] ++ xs
add _ = error "Invalid arguments for add"

sub :: [Value] -> [Value]
sub [] = error "Invalid arguments for sub"
sub [_] = error "Invalid arguments for sub"
sub [IntValue x, IntValue y] = [IntValue (y - x)]
sub (IntValue x:IntValue y:xs) = [IntValue (y - x)] ++ xs
sub _ = error "Invalid arguments for sub"

mul :: [Value] -> [Value]
mul [] = error "Invalid arguments for mul"
mul [_] = error "Invalid arguments for mul"
mul [IntValue x, IntValue y] = [IntValue (y * x)]
mul (IntValue x:IntValue y:xs) = [IntValue (y * x)] ++ xs
mul _ = error "Invalid arguments for mul"

division :: [Value] -> [Value]
division [] = error "Invalid arguments for division"
division [_] = error "Invalid arguments for division"
division [IntValue x, IntValue y] = [IntValue (y `div` x)]
division (IntValue x:IntValue y:xs) = [IntValue (y `div` x)] ++ xs
division _ = error "Invalid arguments for division"

modulo :: [Value] -> [Value]
modulo [] = error "Invalid arguments for mod"
modulo [_] = error "Invalid arguments for mod"
modulo [IntValue x, IntValue y] = [IntValue (y `mod` x)]
modulo (IntValue x:IntValue y:xs) = [IntValue (y `mod` x)] ++ xs
modulo _ = error "Invalid arguments for mod"

eq :: [Value] -> Bool
eq [] = False
eq [BoolValue x] = x
eq [IntValue _] = True
eq [_] = False
eq (BoolValue x:BoolValue y:_) = y == x
eq (IntValue x:IntValue y:_) = y == x
eq _ = False

neq :: [Value] -> Bool
neq [] = False
neq [BoolValue x] = not x
neq [IntValue _] = False
neq [_] = False
neq (BoolValue x:BoolValue y:_) = y /= x
neq (IntValue x:IntValue y:_) = y /= x
neq _ = False

lt :: [Value] -> Bool
lt [] = False
lt [_] = False
lt (IntValue x:IntValue y:_) = y < x
lt _ = False

le :: [Value] -> Bool
le [] = False
le [_] = False
le (IntValue x:IntValue y:_) = y <= x
le _ = False

gt :: [Value] -> Bool
gt [] = False
gt [_] = False
gt (IntValue x:IntValue y:_) = y > x
gt _ = False

ge :: [Value] -> Bool
ge [] = False
ge [_] = False
ge [IntValue x, IntValue y] = y >= x
ge (IntValue x:IntValue y:_) = y >= x
ge _ = False

push :: [Value] -> Value -> [Value]
push xs x = x:xs

-- pushArgs :: [Value] -> [Value] -> [Value]
-- pushArgs xs ys = ys ++ xs

-- pushEnv :: [Value] -> [Value] -> [Value]
-- pushEnv xs ys = ys ++ xs

applyOperator :: Operator -> [Value] -> [Value]
applyOperator Add list = add list
applyOperator Sub list = sub list
applyOperator Mul list = mul list
applyOperator Div list = division list
applyOperator Mod list = modulo list
applyOperator _ _ = error "Unsupported operator"

call :: [Value] -> [Value]
call [] = error "Cannot call"
call (OpValue op:xs) = applyOperator op xs
call _ = error "Cannot call"


jumpIfTrue :: [Value] -> Bool
jumpIfTrue [] = False
jumpIfTrue [_] = False
jumpIfTrue (OpValue Eq:xs) = eq xs
jumpIfTrue (OpValue Ne:xs) = neq xs
jumpIfTrue (OpValue Lt:xs) = lt xs
jumpIfTrue (OpValue Le:xs) = le xs
jumpIfTrue (OpValue Gt:xs) = gt xs
jumpIfTrue (OpValue Ge:xs) = ge xs
jumpIfTrue _ = False

jumpIfFalse :: [Value] -> Bool
jumpIfFalse [] = False
jumpIfFalse [_] = False
jumpIfFalse (OpValue Eq:xs) = neq xs
jumpIfFalse (OpValue Ne:xs) = eq xs
jumpIfFalse (OpValue Lt:xs) = ge xs
jumpIfFalse (OpValue Le:xs) = gt xs
jumpIfFalse (OpValue Gt:xs) = le xs
jumpIfFalse (OpValue Ge:xs) = lt xs
jumpIfFalse _ = False

jump :: Bool -> Int -> [Instruction] -> Int -> Int
jump condition offset instructions currentIndex
  | condition = let targetIndex = currentIndex + offset
                in if targetIndex < 0 || targetIndex >= length instructions
                     then error "Jump out of bounds"
                     else targetIndex
  | otherwise = currentIndex + 1  -- Proceed to the next instruction


execute :: [Instruction] -> [Value] -> Int -> Int
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