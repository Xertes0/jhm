module Execution.Execute
  ( executeMain
  ) where

import Control.Monad.Trans.State.Lazy
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B

import Execution.Code
import Execution.Frame
import Type.AccessFlag
import Type.Attribute
import Type.ClassFile
import Type.ConstantPool
import Type.Method
import Utility.Assert

findMain :: ClassFile -> Method
findMain clsf =
  head
    . assertWith
        "Could not find or found too many 'public static main(String[] args)' methods"
        ((== 1) . length)
    $ filter ((== "main") . (snd . ($ cpool) . runCPRef . mthdName))
    $ filter
        ((== "([Ljava/lang/String;)V") . (snd . ($ cpool) . runCPRef . mthdDesc))
    $ filter
        ((\x -> AccStatic `elem` x && AccPublic `elem` x) . mthdAccessFlags)
    $ clsfMethods clsf
  where
    cpool = clsfConstantPool clsf

executeMain :: ClassFile -> [IO ()]
executeMain clsf =
  runGet
    (evalStateT executeCode (clsf, mkFrame mainCodeAttr))
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
        $ findMain clsf
