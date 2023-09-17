module Type.Attribute
  ( AttributeInfo(..)
  , Attribute(..)
  , isAttrCode
  , getAttributes
  ) where

import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Data.Word

import Type.ConstantPool

type Code = [Word8]

data LineNumberEntry = LineNumberEntry
  { lineNumEntryStartPc :: Word16
  , lineNumEntryLineNum :: Word16
  } deriving (Show)

data ExceptionTable = ExceptionTable
  { startPc :: Word16
  , endPc :: Word16
  , handlerPc :: Word16
  , catchType :: Word16
  } deriving (Show)

getExceptionTable :: Get ExceptionTable
getExceptionTable =
  ExceptionTable <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be

getExceptionTables :: Get [ExceptionTable]
getExceptionTables = do
  count <- fromIntegral <$> getWord16be
  replicateM count getExceptionTable

data AttributeInfo
  = AttrCode
      { attrInfoMaxStack :: Word16
      , attrInfoMaxLocals :: Word16
      , attrInfoCode :: Code
      , attrInfoExceptionTables :: [ExceptionTable]
      , attrInfoAttributes :: [Attribute]
      }
  | AttrLineNumberTable
      { attrLineNumTab :: [LineNumberEntry]
      }
  deriving (Show)

isAttrCode :: AttributeInfo -> Bool
isAttrCode (AttrCode {}) = True
isAttrCode _ = False

getCode :: Get Code
getCode = do
  count <- fromIntegral <$> getWord32be
  replicateM count getWord8

getLineNumberEntry :: Get LineNumberEntry
getLineNumberEntry = LineNumberEntry <$> getWord16be <*> getWord16be

getLineNumberTable :: Get AttributeInfo
getLineNumberTable = do
  count <- fromIntegral <$> getWord16be
  AttrLineNumberTable <$> replicateM count getLineNumberEntry

codeFromWords :: [Word8] -> AttributeInfo
codeFromWords =
  runGet
    (AttrCode
       <$> getWord16be
       <*> getWord16be
       <*> getCode
       <*> getExceptionTables
       <*> getAttributes)
    . B.pack

lineNumberTableFromWords :: [Word8] -> AttributeInfo
lineNumberTableFromWords = runGet getLineNumberTable . B.pack

mkCPRefAttrInfo :: Word16 -> [Word8] -> CPRef AttributeInfo
mkCPRefAttrInfo nameIdx wrds = f <$> mkCPRefUtf8 nameIdx
  where
    f name =
      case name of
        "Code" -> codeFromWords wrds
        "LineNumberTable" -> lineNumberTableFromWords wrds
        _ -> error $ "Unknown attribute: '" ++ name ++ "'"

newtype Attribute = Attribute
  { attrInfo :: CPRef AttributeInfo
  } deriving (Show)

getAttribute :: Get Attribute
getAttribute = do
  nameIdx <- getWord16be
  count <- fromIntegral <$> getWord32be
  info <- replicateM count getWord8
  return $ Attribute (mkCPRefAttrInfo nameIdx info)

getAttributes :: Get [Attribute]
getAttributes = do
  count <- fromIntegral <$> getWord16be
  replicateM count getAttribute
