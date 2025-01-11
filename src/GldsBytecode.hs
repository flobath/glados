module GldsBytecode (
    writeProgramToFile,
    readProgramFromFile
) where

import StackMachine (Value(..), Operator(..), StackInstruction(..), StackProgram)
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
serializeOperator And = putWord8 11
serializeOperator Or = putWord8 12

serializeInstruction :: StackInstruction -> Put
serializeInstruction (PushValue value) = do
    putWord8 0
    serializeValue value
serializeInstruction (PushEnv name) = do
    putWord8 2
    putByteString (BL.toStrict (BL.pack (map (fromIntegral . fromEnum) name ++ [0])))
serializeInstruction (StoreEnv name) = do
    putWord8 3
    putByteString (BL.toStrict (BL.pack (map (fromIntegral . fromEnum) name ++ [0])))
serializeInstruction (Call n) = do
    putWord8 4
    putInt32le (fromIntegral n)
serializeInstruction Return = putWord8 5
serializeInstruction (Jump n) = do
    putWord8 6
    putInt32le (fromIntegral n)
serializeInstruction (JumpIfFalse n) = do
    putWord8 7
    putInt32le (fromIntegral n)
serializeInstruction (OpValue op) = do
    putWord8 8
    serializeOperator op
serializeInstruction _ = error "Not implemented"

serializeProgram :: StackProgram -> Put
serializeProgram = mapM_ serializeInstruction

writeProgramToFile :: FilePath -> StackProgram -> IO ()
writeProgramToFile path program = BL.writeFile path (runPut $ do
  mapM_ putWord8 [7, 12, 4, 19]
  serializeProgram program)



-- Deserialize

deserializeValue :: Get Value
deserializeValue = do
    tag <- getWord8
    case tag of
        0 -> IntValue . fromIntegral <$> getInt32le
        1 -> BoolValue . (/= 0) <$> getWord8
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
        11 -> return And
        12 -> return Or
        _ -> fail "Unknown Operator tag"

deserializeInstruction :: Get StackInstruction
deserializeInstruction = do
    tag <- getWord8
    case tag of
        0 -> PushValue <$> deserializeValue
        2 -> PushEnv <$> getNullTerminatedString
        3 -> StoreEnv <$> getNullTerminatedString
        4 -> Call . fromIntegral <$> getInt32le
        5 -> return Return
        6 -> Jump . fromIntegral <$> getInt32le
        7 -> JumpIfFalse . fromIntegral <$> getInt32le
        8 -> OpValue <$> deserializeOperator
        _ -> fail "Unknown Instruction tag"

deserializeProgram :: Get StackProgram
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

readProgramFromFile :: FilePath -> IO StackProgram
readProgramFromFile path = do
    bytecode <- BL.readFile path
    let magic = runGet (replicateM 4 getWord8) bytecode
    if magic /= [0x47, 0x4C, 0x44, 0x53]
        then error "Invalid magic number"
        else return $ runGet deserializeProgram (BL.drop 4 bytecode)
