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
      { clsNameIndex :: Word16
      }
  | CPFieldref
      { fldrClassIndex :: Word16
      , fldrNameAndTypeIndex :: Word16
      }
  | CPMethodref
      { mthdrClassIndex :: Word16
      , mthdrNameAndTypeIndex :: Word16
      }
  | CPString
      { strStringIndex :: Word16
      }
  | CPNameAndType
      { natNameIndex :: Word16
      , natDescIndex :: Word16
      }
  | CPUtf8
      { utfBytes :: [Word8]
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
  = AccAbstract
  | AccAnnotation
  | AccBridge
  | AccEnum
  | AccFinal
  | AccInterface
  | AccNative
  | AccPrivate
  | AccProtected
  | AccPublic
  | AccStatic
  | AccStrict
  | AccSuper
  | AccSynchronized
  | AccSynthetic
  | AccVarargs
  deriving (Show)

accessFlagMap :: [(Word16, AccessFlag)]
accessFlagMap =
  [ (0x0001, AccPublic)
  , (0x0002, AccPrivate)
  , (0x0004, AccProtected)
  , (0x0008, AccStatic)
  , (0x0010, AccFinal)
  , (0x0020, AccSuper)
  , (0x0020, AccSynchronized)
  , (0x0040, AccBridge)
  , (0x0080, AccVarargs)
  , (0x0100, AccNative)
  , (0x0200, AccInterface)
  , (0x0400, AccAbstract)
  , (0x0800, AccStrict)
  , (0x1000, AccSynthetic)
  , (0x2000, AccAnnotation)
  , (0x4000, AccEnum)
  ]

getAccessFlags :: Get [AccessFlag]
getAccessFlags = do
  value <- getWord16be
  return $ map snd $ filter ((/= 0) . (.&. value) . fst) accessFlagMap

getInterfaces :: Get [Word16]
getInterfaces = do
  count <- fromIntegral <$> getWord16be
  replicateM count getWord16be

data Attribute = Attribute
  { attrNameIndex :: Word16
  , attrInfo :: [Word8]
  } deriving (Show)

getAttribute :: Get Attribute
getAttribute = do
  nameIdx <- getWord16be
  count <- fromIntegral <$> getWord32be
  info <- replicateM count getWord8
  return $ Attribute nameIdx info

getAttributes :: Get [Attribute]
getAttributes = do
  count <- fromIntegral <$> getWord16be
  replicateM count getAttribute

data Field = Field
  { fldAccessFlags :: [AccessFlag]
  , fldNameIndex :: Word16
  , fldDescIndex :: Word16
  , fldAttributes :: [Attribute]
  } deriving (Show)

getField :: Get Field
getField = Field <$> getAccessFlags <*> getWord16be <*> getWord16be <*> getAttributes

getFields :: Get [Field]
getFields = do
  count <- fromIntegral <$> getWord16be
  replicateM count getField

data Method = Method
  { mthdAccessFlags :: [AccessFlag]
  , mthdNameIndex :: Word16
  , mthdDescIndex :: Word16
  , mthdAttibutes :: [Attribute]
  } deriving (Show)

getMethod :: Get Method
getMethod =
  Method <$> getAccessFlags <*> getWord16be <*> getWord16be <*> getAttributes

getMethods :: Get [Method]
getMethods = do
  count <- fromIntegral <$> getWord16be
  replicateM count getMethod

data ClassFile = ClassFile
  { clsfMinor :: Word16
  , clsfMajor :: Word16
  , clsfConstantPool :: ConstantPool
  , clsfAccessFlags :: [AccessFlag]
  , clsfThisClass :: Word16
  , clsfSuperClass :: Word16
  , clsfInterfaces :: [Word16]
  , clsfFields :: [Field]
  , clsfMethods :: [Method]
  , clsfAttributes :: [Attribute]
  } deriving (Show)

getClassFile :: Get ClassFile
getClassFile =
  ClassFile
    <$> (validateMagic *> getWord16be)
    <*> getWord16be
    <*> getConstantPool
    <*> getAccessFlags
    <*> getWord16be
    <*> getWord16be
    <*> getInterfaces
    <*> getFields
    <*> getMethods
    <*> getAttributes

parseClassFile :: IO ClassFile
parseClassFile = do
  file <- B.readFile "Main.class"
  return $ runGet getClassFile file

main :: IO ()
main = putStrLn "Hello, Haskell!"
