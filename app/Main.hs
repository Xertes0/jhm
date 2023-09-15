module Main where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word

data CPInfo
  = CPClass
      { nameIndex :: Word16
      }
  | CPFieldref
      { classIndex :: Word16
      , nameAndTypeIndex :: Word16
      }
  | CPMethodref
      { classIndex :: Word16
      , nameAndTypeIndex :: Word16
      }
  | CPString
      { stringIndex :: Word16
      }
  | CPNameAndType
      { nameIndex :: Word16
      , descIndex :: Word16
      }
  | CPUtf8
      { bytes :: [Word8]
      }
  deriving (Show)

getCPClass :: Get CPInfo
getCPClass = CPClass <$> getWord16be

getCPFieldref :: Get CPInfo
getCPFieldref = CPFieldref <$> getWord16be <*> getWord16be

getCPMethodref :: Get CPInfo
getCPMethodref = CPMethodref <$> getWord16be <*> getWord16be

getCPString :: Get CPInfo
getCPString = CPString <$> getWord16be

getCPNameAndType :: Get CPInfo
getCPNameAndType = CPNameAndType <$> getWord16be <*> getWord16be

getCPUtf8 :: Get CPInfo
getCPUtf8 = do
  count <- fromIntegral <$> getWord16be
  CPUtf8 <$> (sequence $ replicate count getWord8)

getCPInfoByTag :: Word8 -> Get CPInfo
getCPInfoByTag 7 = getCPClass
getCPInfoByTag 9 = getCPFieldref
getCPInfoByTag 10 = getCPMethodref
getCPInfoByTag 8 = getCPString
getCPInfoByTag 12 = getCPNameAndType
getCPInfoByTag 1 = getCPUtf8
getCPInfoByTag _ = error "Unknown info tag"

getCPInfo :: Get CPInfo
getCPInfo = do
  tag <- getWord8
  getCPInfoByTag tag

type ConstantPool = [CPInfo]

getConstantPool :: Get ConstantPool
getConstantPool = do
  count <- ((+) (-1 :: Int)) <$> fromIntegral <$> getWord16be
  sequence $ replicate count getCPInfo

data ClassFile = ClassFile
  { magic :: Word32
  , minor :: Word16
  , major :: Word16
  , constantPool :: ConstantPool
  } deriving (Show)

getClassFile :: Get ClassFile
getClassFile =
  ClassFile <$> getWord32be <*> getWord16be <*> getWord16be <*> getConstantPool

parseClassFile :: IO ClassFile
parseClassFile = do
  file <- B.readFile "Main.class"
  return $ runGet getClassFile file

main :: IO ()
main = putStrLn "Hello, Haskell!"
