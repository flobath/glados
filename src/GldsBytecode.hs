module GldsBytecode (
    writeProgramToFile,
    readProgramFromFile
) where

import StackMachine (Value(..), Operator(..), Instruction(..), Program)
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad (replicateM)
import Data.Word (Word8)

-- Serialize

serializeValue :: Value -> Put
serializeValue (IntValue i) = do
    putWord8 0
    putInt32le (fromIntegral i)
serializeValue (BoolValue b) = do
    putWord8 1
    putWord8 (if b then 1 else 0)
serializeValue (OpValue op) = do
    putWord8 2
    serializeOperator op
serializeValue (FuncValue func) = do
    putWord8 3
    serializeProgram func

serializeOperator :: Operator -> Put
serializeOperator Add = putWord8 0
serializeOperator Sub = putWord8 1
serializeOperator Mul = putWord8 2
serializeOperator Div = putWord8 3
serializeOperator Mod = putWord8 4
serializeOperator Eq = putWord8 5
serializeOperator Ne = putWord8 6
serializeOperator Lt = putWord8 7
serializeOperator Le = putWord8 8
serializeOperator Gt = putWord8 9
serializeOperator Ge = putWord8 10

serializeInstruction :: Instruction -> Put
serializeInstruction (Push val) = do
    putWord8 0
    serializeValue val
serializeInstruction (PushArgs n) = do
    putWord8 1
    putInt32le (fromIntegral n)
serializeInstruction (PushEnv name) = do
    putWord8 2
    mapM_ putWord8 (map (fromIntegral . fromEnum) name)
    putWord8 0 -- Null terminator for the string
serializeInstruction Call = putWord8 3
serializeInstruction Return = putWord8 4
serializeInstruction (JumpIfTrue n) = do
    putWord8 5
    putInt32le (fromIntegral n)
serializeInstruction (JumpIfFalse n) = do
    putWord8 6
    putInt32le (fromIntegral n)

serializeProgram :: Program -> Put
serializeProgram = mapM_ serializeInstruction

writeProgramToFile :: FilePath -> Program -> IO ()
writeProgramToFile path program = BL.writeFile path (runPut $ do
  mapM_ putWord8 [0x47, 0x4C, 0x44, 0x53] -- Magic number
  serializeProgram program)



-- Deserialize

deserializeValue :: Get Value
deserializeValue = do
    tag <- getWord8
    case tag of
        0 -> IntValue . fromIntegral <$> getInt32le
        1 -> BoolValue . (/= 0) <$> getWord8
        2 -> OpValue <$> deserializeOperator
        3 -> FuncValue <$> deserializeProgram
        _ -> fail "Unknown Value tag"

deserializeOperator :: Get Operator
deserializeOperator = do
    tag <- getWord8
    case tag of
        0 -> return Add
        1 -> return Sub
        2 -> return Mul
        3 -> return Div
        4 -> return Mod
        5 -> return Eq
        6 -> return Ne
        7 -> return Lt
        8 -> return Le
        9 -> return Gt
        10 -> return Ge
        _ -> fail "Unknown Operator tag"

deserializeInstruction :: Get Instruction
deserializeInstruction = do
    tag <- getWord8
    case tag of
        0 -> Push <$> deserializeValue
        1 -> PushArgs . fromIntegral <$> getInt32le
        2 -> PushEnv <$> getNullTerminatedString
        3 -> return Call
        4 -> return Return
        5 -> JumpIfTrue . fromIntegral <$> getInt32le
        6 -> JumpIfFalse . fromIntegral <$> getInt32le
        _ -> fail "Unknown Instruction tag"

deserializeProgram :: Get Program
deserializeProgram = do
    empty <- isEmpty
    if empty
        then return []
        else do
            instr <- deserializeInstruction
            rest <- deserializeProgram
            return (instr : rest)

getNullTerminatedString :: Get String
getNullTerminatedString = do
    bytes <- getBytesUntilNull
    return $ map (toEnum . fromIntegral) bytes

getBytesUntilNull :: Get [Word8]
getBytesUntilNull = do
    byte <- getWord8
    if byte == 0
        then return []
        else do
            rest <- getBytesUntilNull
            return (byte : rest)

-- Read Program from Bytecode File
readProgramFromFile :: FilePath -> IO Program
readProgramFromFile path = do
    bytecode <- BL.readFile path
    let magic = runGet (replicateM 4 getWord8) bytecode
    if magic /= [0x47, 0x4C, 0x44, 0x53]
        then error "Invalid magic number"
        else return $ runGet deserializeProgram (BL.drop 4 bytecode)
