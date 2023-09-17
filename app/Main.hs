module Main
  ( main
  ) where

import Execution.Execute
import Type.ClassFile

main :: IO ()
main = (sequence_ . executeMain) =<< parseClassFile
