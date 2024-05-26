module Ganache.Class.Print
  ( Print (..)
  )
where

import Data.ByteString (ByteString)

class Print a where
  print :: a -> ByteString
