module Type.Method
  ( Method(..)
  , getMethods
  ) where

import Control.Monad
import Data.Binary.Get

import Type.AccessFlag
import Type.Attribute
import Type.ConstantPool

data Method = Method
  { mthdAccessFlags :: [AccessFlag]
  , mthdName :: CPRef String
  , mthdDesc :: CPRef String
  , mthdAttibutes :: [Attribute]
  } deriving (Show)

getMethod :: Get Method
getMethod =
  Method
    <$> getAccessFlags
    <*> (mkCPRefUtf8 <$> getWord16be)
    <*> (mkCPRefUtf8 <$> getWord16be)
    <*> getAttributes

getMethods :: Get [Method]
getMethods = do
  count <- fromIntegral <$> getWord16be
  replicateM count getMethod
