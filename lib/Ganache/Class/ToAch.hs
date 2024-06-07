module Ganache.Class.ToAch
  ( ToAch (..)
  )
where

import Data.ByteString (ByteString)

class ToAch a where
  toAch :: a -> ByteString
