module Execution.Frame
  ( Frame
  , FrameState
  , Value(..)
  , mkFrame
  , invokeMethod
  , getCpool
  , storeLocal
  , getLocal
  , pushStack
  , popStack
  ) where

import Control.Monad.Trans.State.Lazy
import Data.Binary.Get

import Type.Attribute
import Type.ConstantPool
import Type.ClassFile

type FrameState a = StateT (ClassFile, Frame) Get a

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

getCpool :: FrameState ConstantPool
getCpool = do
  (clsf, _) <- get
  return $ clsfConstantPool clsf

pushStack :: Value -> FrameState ()
pushStack val = do
  (clsf, frame) <- get
  put (clsf, frame {frmStack = frmStack frame ++ [val]})

popStack :: FrameState Value
popStack = do
  (clsf, frame) <- get
  put (clsf, frame {frmStack = tail $ frmStack frame})
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

storeLocal :: Int -> Value -> FrameState ()
storeLocal idx val = do
  (clsf, frame) <- get
  let (h, t) = splitAt idx $ frmLocals frame
  put (clsf, frame {frmLocals = h ++ [val] ++ tail t})

getLocal :: Int -> FrameState Value
getLocal idx = do
  (_, frame) <- get
  return $ frmLocals frame !! idx

invokeMethod :: CPInfo -> FrameState (IO ())
invokeMethod (CPMethodref cls nat) = do
  cpool <- getCpool
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
    _ -> error $ "Unknown method: '" ++ methodSig ++ "'"

invokeMethod _ = error "Supplied reference is not a method"
