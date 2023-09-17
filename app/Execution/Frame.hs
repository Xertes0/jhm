module Execution.Frame
  ( Frame(..)
  , FrameState
  , Value(..)
  , mkFrame
  , getClassFile
  , getCpool
  , storeLocal
  , getLocal
  , pushStack
  , concatStack
  , popStack
  , emptyStack
  , getStack
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
  deriving (Show)
  -- | LocalRef
  --     { localRef ::
  --     }
  -- | LocalReturnAddress
  --   { localRetAddr ::
  --   }

getClassFile :: FrameState ClassFile
getClassFile = fst <$> get

getCpool :: FrameState ConstantPool
getCpool = do
  (clsf, _) <- get
  return $ clsfConstantPool clsf

pushStack :: Value -> FrameState ()
pushStack val = do
  (clsf, frame) <- get
  put (clsf, frame {frmStack = val : frmStack frame})

concatStack :: [Value] -> FrameState ()
concatStack vals = do
  (clsf, frame) <- get
  put (clsf, frame {frmStack = vals ++ frmStack frame})

popStack :: FrameState Value
popStack = do
  (clsf, frame) <- get
  put (clsf, frame {frmStack = tail $ frmStack frame})
  return $ head $ frmStack frame

emptyStack :: FrameState ()
emptyStack = do
  (clsf, frame) <- get
  put (clsf, frame {frmStack = []})
  return ()

getStack :: FrameState [Value]
getStack = do
  (_, frame) <- get
  return $ frmStack frame

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
