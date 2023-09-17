module Utility.Assert
  ( assertWith
  ) where

assertWith :: String -> (a -> Bool) -> a -> a
assertWith msg f a =
  if f a
    then a
    else error msg
