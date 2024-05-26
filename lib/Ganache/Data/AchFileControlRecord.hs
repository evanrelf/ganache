module Ganache.Data.AchFileControlRecord
  ( AchFileControlRecord (..)
  )
where

import Data.ByteString (ByteString)

newtype AchFileControlRecord = AchFileControlRecord ByteString
