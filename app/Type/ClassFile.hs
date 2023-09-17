module Type.ClassFile
  ( ClassFile(..)
  , parseClassFile
  ) where

import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Data.Word

import Type.AccessFlag
import Type.Attribute
import Type.ConstantPool
import Type.Field
import Type.Method

validateMagic :: Get Word32
validateMagic = do
  magic <- getWord32be
  if magic /= 0xcafebabe
    then error "Invalid file format"
    else return magic

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

getInterfaces :: Get [Word16]
getInterfaces = do
  count <- fromIntegral <$> getWord16be
  replicateM count getWord16be

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
