module Execution.Frame
  ( Frame
  , Value(..)
  , mkFrame
  , executeMethod
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

getCpool :: StateT (Frame, ConstantPool) Get ConstantPool
getCpool = do
  (_, cpool) <- get
  return cpool

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
