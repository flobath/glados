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
serializeValue (ListValue xs) = do
    putWord8 2
    putInt32le (fromIntegral $ length xs)
    mapM_ serializeValue xs

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
serializeOperator ListIndex = putWord8 13

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

serializeProgram :: StackProgram -> Put
serializeProgram = mapM_ serializeInstruction

writeProgramToFile :: FilePath -> StackProgram -> IO ()
writeProgramToFile path program = BL.writeFile path (runPut $ do
  mapM_ putWord8 [7, 12, 4, 19]
  serializeProgram program)

-- Deserialize

deserializeValue :: Get (Either String Value)
deserializeValue = do
    tag <- getWord8
    case tag of
        0 -> Right . IntValue . fromIntegral <$> getInt32le
        1 -> Right . BoolValue . (/= 0) <$> getWord8
        2 -> do
            len <- fromIntegral <$> getInt32le
            xs <- replicateM len deserializeValue
            return $ fmap ListValue (sequence xs)
        _ -> return $ Left "Unknown Value tag"

deserializeOperator :: Get (Either String Operator)
deserializeOperator = do
    tag <- getWord8
    case tag of
        0 -> return $ Right Add
        1 -> return $ Right Sub
        2 -> return $ Right Mul
        3 -> return $ Right Div
        4 -> return $ Right Mod
        5 -> return $ Right Eq
        6 -> return $ Right Ne
        7 -> return $ Right Lt
        8 -> return $ Right Le
        9 -> return $ Right Gt
        10 -> return $ Right Ge
        11 -> return $ Right And
        12 -> return $ Right Or
        13 -> return $ Right ListIndex
        _ -> return $ Left "Unknown Operator tag"

deserializeInstruction :: Get (Either String StackInstruction)
deserializeInstruction = do
    tag <- getWord8
    case tag of
        0 -> fmap PushValue <$> deserializeValue
        2 -> fmap PushEnv <$> getNullTerminatedString
        3 -> fmap StoreEnv <$> getNullTerminatedString
        4 -> Right . Call . fromIntegral <$> getInt32le
        5 -> return $ Right Return
        6 -> Right . Jump . fromIntegral <$> getInt32le
        7 -> Right . JumpIfFalse . fromIntegral <$> getInt32le
        8 -> fmap OpValue <$> deserializeOperator
        _ -> return $ Left "Unknown Instruction tag"

deserializeProgram :: Get (Either String StackProgram)
deserializeProgram = do
    empty <- isEmpty
    if empty
        then return $ Right []
        else do
            instr <- deserializeInstruction
            case instr of
                Left err -> return $ Left err
                Right i -> do
                    rest <- deserializeProgram
                    case rest of
                        Left err -> return $ Left err
                        Right r -> return $ Right (i : r)

getNullTerminatedString :: Get (Either String String)
getNullTerminatedString = do
    bytes <- getBytesUntilNull
    return $ Right $ map (toEnum . fromIntegral) bytes

getBytesUntilNull :: Get [Word8]
getBytesUntilNull = do
    byte <- getWord8
    if byte == 0
        then return []
        else do
            rest <- getBytesUntilNull
            return (byte : rest)

readProgramFromFile :: FilePath -> IO (Either String StackProgram)
readProgramFromFile path = do
    bytecode <- BL.readFile path
    let magic = runGet (replicateM 4 getWord8) bytecode
    if magic /= [0x47, 0x4C, 0x44, 0x53]
        then return $ Left "Invalid magic number"
        else return $ runGet deserializeProgram (BL.drop 4 bytecode)

exampleProgram :: StackProgram
exampleProgram = [
    PushValue (ListValue [IntValue 1, IntValue 2, IntValue 3]),
    PushEnv "x",
    OpValue Add,
    StoreEnv "y",
    Return
    ]
