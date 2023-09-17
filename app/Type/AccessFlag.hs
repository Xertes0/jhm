module Type.AccessFlag
  ( AccessFlag(..)
  , getAccessFlags
  ) where

import Data.Binary.Get
import Data.Bits
import Data.Word

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
  deriving (Show, Eq)

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
