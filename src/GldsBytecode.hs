module GldsBytecode (
    writeProgramToFile,
    readProgramFromFile
) where

import StackMachine (Value(..), Operator(..), StackInstruction(..), StackProgram)
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad (replicateM)
import Data.Word (Word8)
import Helpers((?:), (>&<), ffmap)
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding

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

putText :: T.Text -> Put
putText t
    = putByteString (Data.Text.Encoding.encodeUtf8 t)
    >> putWord8 0

serializeInstruction :: StackInstruction -> Put
serializeInstruction (PushValue value) = do
    putWord8 0
    serializeValue value
serializeInstruction (PushEnv name) = do
    putWord8 2
    putText name
serializeInstruction (StoreEnv name) = do
    putWord8 3
    putText name
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
serializeInstruction NewEnv = putWord8 9
serializeInstruction instr = error $
    "Instruction not implemented: " ++ show instr

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
        _ -> return $ Left "Unknown Operator tag"

deserializeInstruction :: Get (Either String StackInstruction)
deserializeInstruction = do
    tag <- getWord8
    case tag of
        0 -> fmap PushValue <$> deserializeValue
        2 -> fmap PushEnv <$> getNullTerminatedText
        3 -> fmap StoreEnv <$> getNullTerminatedText
        4 -> Right . Call . fromIntegral <$> getInt32le
        5 -> return $ Right Return
        6 -> Right . Jump . fromIntegral <$> getInt32le
        7 -> Right . JumpIfFalse . fromIntegral <$> getInt32le
        8 -> fmap OpValue <$> deserializeOperator
        9 -> return $ Right NewEnv
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

getNullTerminatedLazyText :: Get (Either String TL.Text)
getNullTerminatedLazyText = do
    bytes <- getBytesUntilNull
    let lbs = BL.pack bytes
    let eitherT = Data.Text.Lazy.Encoding.decodeUtf8' lbs
    return $ eitherT >&< show

getNullTerminatedText :: Get (Either String T.Text)
getNullTerminatedText = ffmap TL.toStrict getNullTerminatedLazyText

getNullTerminatedString :: Get String
getNullTerminatedString = map (toEnum . fromIntegral) <$> getBytesUntilNull

getBytesUntilNull :: Get [Word8]
getBytesUntilNull = do
    byte <- getWord8
    if byte == 0
        then return []
        else byte ?: getBytesUntilNull

readProgramFromFile :: FilePath -> IO (Either String StackProgram)
readProgramFromFile path = do
    bytecode <- BL.readFile path
    let magic = runGet (replicateM 4 getWord8) bytecode
    if magic /= [0x07, 0x0c, 0x04, 0x13]
        then return $ Left $ path ++ ": Invalid magic number"
        else return $ runGet deserializeProgram (BL.drop 4 bytecode)
