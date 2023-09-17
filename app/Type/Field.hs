module Type.Field
  ( Field(..)
  , getFields
  ) where

import Control.Monad
import Data.Binary.Get

import Type.AccessFlag
import Type.Attribute
import Type.ConstantPool

data Field = Field
  { fldAccessFlags :: [AccessFlag]
  , fldName :: CPRef String
  , fldDesc :: CPRef String
  , fldAttributes :: [Attribute]
  } deriving (Show)

getField :: Get Field
getField =
  Field
    <$> getAccessFlags
    <*> (mkCPRefUtf8 <$> getWord16be)
    <*> (mkCPRefUtf8 <$> getWord16be)
    <*> getAttributes

getFields :: Get [Field]
getFields = do
  count <- fromIntegral <$> getWord16be
  replicateM count getField
