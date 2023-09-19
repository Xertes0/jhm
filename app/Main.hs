module Main
  ( main
  ) where

import Execution.Execute
import Type.ClassFile
import System.Environment

executeClass :: String -> IO ()
executeClass name = (sequence_ . executeMain) =<< parseClassFile name

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then error "Supply class name as an argument."
    else executeClass $ args !! 1
