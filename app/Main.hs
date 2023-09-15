module Main where

import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word

validateMagic :: Get Word32
validateMagic = do
  magic <- getWord32be
  if magic /= 0xcafebabe
    then error "Invalid file format"
    else return magic

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
  CPUtf8 <$> replicateM count getWord8

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
  count <- (+) (-1 :: Int) . fromIntegral <$> getWord16be
  replicateM count getCPInfo

data AccessFlag
  = AccPublic
  | AccFinal
  | AccSuper
  | AccInterface
  | AccAbstract
  | AccSynthetic
  | AccAnnotation
  | AccEnum
  deriving (Show)

accessFlagMap :: [(Word16, AccessFlag)]
accessFlagMap =
  [ (0x0001, AccPublic)
  , (0x0010, AccFinal)
  , (0x0020, AccSuper)
  , (0x0200, AccInterface)
  , (0x0400, AccAbstract)
  , (0x1000, AccSynthetic)
  , (0x2000, AccAnnotation)
  , (0x4000, AccEnum)
  ]

getAccessFlags :: Get [AccessFlag]
getAccessFlags = do
  value <- getWord16be
  return $ map snd $ filter ((/= 0) . (.&. value) . fst) accessFlagMap

data ClassFile = ClassFile
  { minor :: Word16
  , major :: Word16
  , constantPool :: ConstantPool
  , accessFlags :: [AccessFlag]
  } deriving (Show)

getClassFile :: Get ClassFile
getClassFile =
  ClassFile
    <$> (validateMagic *> getWord16be)
    <*> getWord16be
    <*> getConstantPool
    <*> getAccessFlags

parseClassFile :: IO ClassFile
parseClassFile = do
  file <- B.readFile "Main.class"
  return $ runGet getClassFile file

main :: IO ()
main = putStrLn "Hello, Haskell!"
