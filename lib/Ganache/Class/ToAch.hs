module Ganache.Class.ToAch
  ( ToAch (..)
  )
where

import Data.Text (Text)

class ToAch a where
  toAch :: a -> Text
