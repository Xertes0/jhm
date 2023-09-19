module Type.ConstantPool
  ( CPRef(..)
  , mkCPRefCPInfo
  , mkCPRefUtf8
  , CPInfo(..)
  , ConstantPool
  , getConstantPool
  ) where

import Codec.Binary.UTF8.String (decode)
import Control.Monad
import Data.Binary.Get
import Data.Word
import Data.Int

newtype CPRef a = CPRef
  { runCPRef :: ConstantPool -> (Word16, a)
  }

instance Functor CPRef where
  fmap f (CPRef a) =
    CPRef $ \input -> do
      let (idx, a') = a input
      (idx, f a')

instance Show (CPRef a) where
  show _ = "ConstantPool -> (Word16, a)"

mkCPRefUtf8 :: Word16 -> CPRef String
mkCPRefUtf8 idx = CPRef $ \cp -> (idx, utfString $ cp !! fromIntegral idx)

mkCPRefCPInfo :: Word16 -> CPRef CPInfo
mkCPRefCPInfo idx = CPRef $ \cp -> (idx, cp !! fromIntegral idx)

data CPInfo
  = CPClass
      { clsName :: CPRef CPInfo
      }
  | CPFieldref
      { fldrClass :: CPRef CPInfo
      , fldrNameAndType :: CPRef CPInfo
      }
  | CPMethodref
      { mthdrClass :: CPRef CPInfo
      , mthdrNameAndType :: CPRef CPInfo
      }
  | CPString
      { strString :: CPRef CPInfo
      }
  | CPNameAndType
      { natName :: CPRef CPInfo
      , natDesc :: CPRef CPInfo
      }
  | CPUtf8
      { utfString :: String
      }
  | CPInteger
    { cpInfInt :: Int32
    }
  deriving (Show)

getCPClass :: Get CPInfo
getCPClass = CPClass <$> (mkCPRefCPInfo <$> getWord16be)

getCPFieldref :: Get CPInfo
getCPFieldref =
  CPFieldref
    <$> (mkCPRefCPInfo <$> getWord16be)
    <*> (mkCPRefCPInfo <$> getWord16be)

getCPMethodref :: Get CPInfo
getCPMethodref =
  CPMethodref
    <$> (mkCPRefCPInfo <$> getWord16be)
    <*> (mkCPRefCPInfo <$> getWord16be)

getCPString :: Get CPInfo
getCPString = CPString <$> (mkCPRefCPInfo <$> getWord16be)

getCPNameAndType :: Get CPInfo
getCPNameAndType =
  CPNameAndType
    <$> (mkCPRefCPInfo <$> getWord16be)
    <*> (mkCPRefCPInfo <$> getWord16be)

getCPUtf8 :: Get CPInfo
getCPUtf8 = do
  count <- fromIntegral <$> getWord16be
  CPUtf8 <$> (decode <$> replicateM count getWord8)

getCPInteger :: Get CPInfo
getCPInteger = CPInteger <$> getInt32be

getCPInfoByTag :: Word8 -> Get CPInfo
getCPInfoByTag 7 = getCPClass
getCPInfoByTag 9 = getCPFieldref
getCPInfoByTag 10 = getCPMethodref
getCPInfoByTag 8 = getCPString
getCPInfoByTag 12 = getCPNameAndType
getCPInfoByTag 1 = getCPUtf8
getCPInfoByTag 3 = getCPInteger
getCPInfoByTag n = error $ "Unknown info tag '" ++ show n ++ "'"

getCPInfo :: Get CPInfo
getCPInfo = do
  tag <- getWord8
  getCPInfoByTag tag

type ConstantPool = [CPInfo]

getConstantPool :: Get ConstantPool
getConstantPool = do
  count <- (+) (-1 :: Int) . fromIntegral <$> getWord16be
  ([CPUtf8 "Dummy"] ++) <$> replicateM count getCPInfo
