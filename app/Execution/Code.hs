module Execution.Code
  ( executeCode
  ) where

import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class
import Data.Binary.Get
import Data.Word
import Numeric (showHex)

import Execution.Frame

executeInstruction :: Word8 -> FrameState (IO ())

-- bipush
executeInstruction 0x10 = do
  byte <- ValueInt . fromIntegral <$> lift getWord8
  pushStack byte
  return $ pure ()

-- ldc
executeInstruction 0x12 = do
  cpool <- getCpool
  cpIdx <- fromIntegral <$> lift getWord8
  pushStack $ ValueCPInfo $ cpool !! cpIdx
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

-- return
executeInstruction 0xb1 = return $ pure ()

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

executeCode :: FrameState [IO ()]
executeCode =
  whileM (not <$> lift isEmpty) $ do
    opcode <- lift getWord8
    executeInstruction opcode
