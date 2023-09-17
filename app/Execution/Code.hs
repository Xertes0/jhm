module Execution.Code
  ( executeCode
  ) where

import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word
import Numeric (showHex)

import Execution.Frame
import Type.Attribute
import Type.ConstantPool
import Type.ClassFile
import Type.Method
import Utility.Assert

iloadN :: Int -> FrameState (IO ())
iloadN n = do
  -- TODO: Assert that the local is ValueInt
  pushStack =<< getLocal n
  return $ pure ()

istoreN :: Int -> FrameState (IO ())
istoreN n = do
  storeLocal n =<< popStack
  return $ pure ()

executeInstruction :: Word8 -> FrameState (IO ())

-- bipush
executeInstruction 0x10 = do
  byte <- ValueInt . fromIntegral <$> lift getInt8
  pushStack byte
  return $ pure ()

-- ldc
executeInstruction 0x12 = do
  cpool <- getCpool
  cpIdx <- fromIntegral <$> lift getWord8
  pushStack $ ValueCPInfo $ cpool !! cpIdx
  return $ pure ()

-- iload_n
executeInstruction 0x1a = iloadN 0
executeInstruction 0x1b = iloadN 1
executeInstruction 0x1c = iloadN 2
executeInstruction 0x1d = iloadN 3

-- istore
executeInstruction 0x36 = do
  idx <- fromIntegral <$> lift getWord8
  storeLocal idx =<< popStack
  return $ pure ()

-- istore_n
executeInstruction 0x3b = istoreN 0
executeInstruction 0x3c = istoreN 1
executeInstruction 0x3d = istoreN 2
executeInstruction 0x3e = istoreN 3

-- iadd
executeInstruction 0x60 = do
  b <- valInt <$> popStack
  a <- valInt <$> popStack
  pushStack $ ValueInt $ a + b
  return $ pure ()

-- iinc
executeInstruction 0x84 = do
  cpIdx <- fromIntegral <$> lift getWord8
  incValue <- fromIntegral <$> lift getInt8
  (ValueInt oldLoc) <- getLocal cpIdx
  storeLocal cpIdx $ ValueInt $ oldLoc + incValue
  return $ pure ()

-- ireturn
executeInstruction 0xac = do
  retVal <- popStack
  emptyStack
  pushStack retVal
  return $ pure ()

-- return
executeInstruction 0xb1 = do
  emptyStack
  return $ pure ()

-- getstatic
executeInstruction 0xb2 = do
  cpool <- getCpool
  cpIdx <- fromIntegral <$> lift getWord16be
  pushStack $ ValueCPInfo $ cpool !! cpIdx
  return $ pure ()

-- invokevirtual
executeInstruction 0xb6 = do
  cpool <- getCpool
  methodRef <- (cpool !!) . fromIntegral <$> lift getWord16be
  invokeMethod methodRef

-- invokestatic
executeInstruction 0xb8 = do
  cpool <- getCpool
  methodRef <- (cpool !!) . fromIntegral <$> lift getWord16be
  invokeMethod methodRef

executeInstruction ins = error $ "Unknown instruction: '0x" ++ showHex ins "'"

invokeMethod :: CPInfo -> FrameState (IO ())
invokeMethod (CPMethodref cls nat) = do
  clsf <- getClassFile
  let cpool = clsfConstantPool clsf
  let cls' = snd $ runCPRef cls cpool
  let className = utfString $ snd $ runCPRef (clsName cls') cpool
  let nat' = snd $ runCPRef nat cpool
  let methodName = utfString $ snd $ runCPRef (natName nat') cpool
  let methodType = utfString $ snd $ runCPRef (natDesc nat') cpool
  let methodSig = className ++ "." ++ methodName ++ ":" ++ methodType
  case methodSig of
    "java/io/PrintStream.println:(Ljava/lang/String;)V" -> do
      _ <- popStack
      putStrLn . utfString . snd . flip runCPRef cpool . strString . valCPInfo
        <$> popStack
    "java/io/PrintStream.println:(I)V" -> do
      _ <- popStack
      print . valInt <$> popStack
    _ -> do
      -- This assumes that the method is of the same class.
      let mthd = findMethod clsf $ tail $ dropWhile (/= '.') methodSig
      let mthdCodeAttr = findAttribute cpool $ mthdAttibutes mthd
      myStack <- getStack
      let argCount = countArguments $ snd $ runCPRef (mthdDesc mthd) cpool
      let mthdFrame = mkFrame mthdCodeAttr
      let mthdFrame' =
            mthdFrame
              { frmLocals =
                  (drop (length myStack - argCount) myStack)
                    ++ (drop argCount $ frmLocals mthdFrame)
              }
      let (io, (_, retFrame)) =
            runGet
              (runStateT executeCode (clsf, mthdFrame'))
              (B.pack $ attrInfoCode mthdCodeAttr)
      concatStack $ frmStack retFrame
      return $ sequence_ io

invokeMethod _ = error "Supplied reference is not a method"

findMethod :: ClassFile -> String -> Method
findMethod clsf signature =
  head
    $ assertWith
        ("Could not find method by signature: '" ++ signature ++ "'")
        ((== 1) . length)
        (filter
           ((== signature)
              . (\[n, d] -> n ++ ":" ++ d)
              . fmap (snd . ($ cpool) . runCPRef)
              . flip fmap [mthdName, mthdDesc]
              . flip ($))
           $ clsfMethods clsf)
  where
    cpool = clsfConstantPool clsf

findAttribute :: ConstantPool -> [Attribute] -> AttributeInfo
findAttribute cpool =
  snd
    . ($ cpool)
    . runCPRef
    . attrInfo
    . head
    . assertWith
        "Could not find or found too many 'Code' attributes for the method"
        ((== 1) . length)
    . filter (isAttrCode . snd . ($ cpool) . runCPRef . attrInfo)

executeCode :: FrameState [IO ()]
executeCode =
  whileM (not <$> lift isEmpty) $ do
    opcode <- lift getWord8
    executeInstruction opcode

-- This is not a proper parser. It only works on primitives
countArguments :: String -> Int
countArguments = length . drop 1 . dropWhile (/= '(') . takeWhile (/= ')')
