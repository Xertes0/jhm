module Main where

import Codec.Binary.UTF8.String (decode)
import Control.Monad
import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word
import Numeric (showHex)

assertWith :: String -> (a -> Bool) -> a -> a
assertWith msg f a =
  if f a
    then a
    else error msg

validateMagic :: Get Word32
validateMagic = do
  magic <- getWord32be
  if magic /= 0xcafebabe
    then error "Invalid file format"
    else return magic

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
  ([CPUtf8 "Dummy"] ++) <$> replicateM count getCPInfo

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

getInterfaces :: Get [Word16]
getInterfaces = do
  count <- fromIntegral <$> getWord16be
  replicateM count getWord16be

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

type Code = [Word8]

data LineNumberEntry = LineNumberEntry
  { lineNumEntryStartPc :: Word16
  , lineNumEntryLineNum :: Word16
  } deriving (Show)

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

getMain :: ClassFile -> Method
getMain clsf =
  head
    . assertWith
        "Could not find or found too many 'public static main' methods"
        ((== 1) . length)
    $ filter ((== "main") . (snd . ($ cpool) . runCPRef . mthdName))
    $ filter
        ((\x -> AccStatic `elem` x && AccPublic `elem` x) . mthdAccessFlags)
    $ clsfMethods clsf
  where
    cpool = clsfConstantPool clsf

data Value
  = ValueBoolean
      { valBool :: Bool
      }
  | ValueInt
      { valInt :: Int
      }
  | ValueShort
      { valShort :: Int
      }
  | ValueFloat
      { valFloat :: Float
      }
  | ValueCPInfo
      { valCPInfo :: CPInfo
      }
  -- | LocalRef
  --     { localRef ::
  --     }
  -- | LocalReturnAddress
  --   { localRetAddr ::
  --   }

pushStack :: Value -> StateT (Frame, ConstantPool) Get ()
pushStack val = do
  (frame, cpool) <- get
  put (frame {frmStack = frmStack frame ++ [val]}, cpool)

popStack :: StateT (Frame, ConstantPool) Get Value
popStack = do
  (frame, cpool) <- get
  put (frame {frmStack = tail $ frmStack frame}, cpool)
  return $ head $ frmStack frame

data Frame = Frame
  { frmStack :: [Value]
  , frmLocals :: [Value]
  }

mkFrame :: AttributeInfo -> Frame
mkFrame (AttrCode {attrInfoMaxLocals = maxLocals}) =
  Frame
    { frmStack = []
    , frmLocals = replicate (fromIntegral maxLocals) $ ValueInt 0
    }
mkFrame _ = error "Supplied attribute is not 'Code'"

storeLocal :: Int -> Value -> StateT (Frame, ConstantPool) Get ()
storeLocal idx val = do
  (frame, cpool) <- get
  let (h, t) = splitAt idx $ frmLocals frame
  put (frame {frmLocals = h ++ [val] ++ tail t}, cpool)

getLocal :: Int -> StateT (Frame, ConstantPool) Get Value
getLocal idx = do
  (frame, _) <- get
  return $ frmLocals frame !! idx

executeMethod :: CPInfo -> StateT (Frame, ConstantPool) Get (IO ())
executeMethod (CPMethodref cls nat) = do
  (_, cpool) <- get
  let cls' = snd $ runCPRef cls cpool
  let className = utfString $ snd $ runCPRef (clsName cls') cpool
  let nat' = snd $ runCPRef nat cpool
  let methodName = utfString $ snd $ runCPRef (natName nat') cpool
  let methodType = utfString $ snd $ runCPRef (natDesc nat') cpool
  let methodSig = className ++ "." ++ methodName ++ ":" ++ methodType
  case methodSig of
    "java/io/PrintStream.println:(Ljava/lang/String;)V" -> do
      _ <- popStack
      pure
        . putStrLn
        . utfString
        . snd
        . (flip runCPRef) cpool
        . strString
        . valCPInfo
        =<< popStack
    "java/io/PrintStream.println:(I)V" -> do
      _ <- popStack
      pure . putStrLn . show . valInt =<< popStack
    _ -> error $ "Unknown method: '" ++ methodSig ++ "'"

executeMethod _ = error "Supplied reference is not a method"

executeInstruction :: Word8 -> StateT (Frame, ConstantPool) Get (IO ())

-- bipush
executeInstruction 0x10 = do
  byte <- ValueInt . fromIntegral <$> lift getWord8
  pushStack byte
  return $ pure ()

-- ldc
executeInstruction 0x12 = do
  (frame, cpool) <- get
  cpIdx <- fromIntegral <$> lift getWord8
  put (frame {frmStack = frmStack frame ++ [ValueCPInfo $ cpool !! cpIdx]}, cpool)
  return $ pure ()

-- iload_n
executeInstruction 0x1b = do
  -- TODO: Assert that the local is ValueInt
  pushStack =<< getLocal 1
  return $ pure ()

-- istore
executeInstruction 0x36 = do
  idx <- fromIntegral <$> lift getWord8
  storeLocal idx =<< popStack
  return $ pure ()

-- istore_n
executeInstruction 0x3c = do
  storeLocal 1 =<< popStack
  return $ pure ()

-- iinc
executeInstruction 0x84 = do
  cpIdx <- fromIntegral <$> lift getWord8
  incValue <- fromIntegral <$> lift getWord8
  (ValueInt oldLoc) <- getLocal cpIdx
  storeLocal cpIdx $ ValueInt $ oldLoc + incValue
  return $ pure ()

-- getstatic
executeInstruction 0xb2 = do
  (_, cpool) <- get
  cpIdx <- fromIntegral <$> lift getWord16be
  pushStack $ ValueCPInfo $ cpool !! cpIdx
  return $ pure ()

-- return
executeInstruction 0xb1 = return $ pure ()

-- invokevirtual
executeInstruction 0xb6 = do
  (_, cpool) <- get
  methodRef <- (cpool !!) . fromIntegral <$> lift getWord16be
  executeMethod methodRef

executeInstruction ins = error $ "Unknown instruction: '0x" ++ showHex ins "'"

executeCode :: StateT (Frame, ConstantPool) Get [IO ()]
executeCode = whileM (not <$> lift isEmpty) $ do
  opcode <- lift getWord8
  executeInstruction opcode

executeMain :: ClassFile -> [IO ()]
executeMain clsf =
  runGet
    (evalStateT executeCode (mkFrame mainCodeAttr, cpool))
    (B.pack $ attrInfoCode mainCodeAttr)
  where
    cpool = clsfConstantPool clsf
    mainCodeAttr =
      snd
        $ ($ cpool)
        $ runCPRef
        $ attrInfo
        $ head
        $ assertWith
            "Could not find or found too many 'Code' attributes for the main method"
            ((== 1) . length)
        $ filter (isAttrCode . snd . ($ cpool) . runCPRef . attrInfo)
        $ mthdAttibutes
        $ getMain clsf

main :: IO ()
main = (sequence_ . executeMain) =<< parseClassFile
