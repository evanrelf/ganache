module Ganache.Data.AchBatchControlRecord
  ( AchBatchControlRecord (..)
  )
where

import Data.ByteString (ByteString)

newtype AchBatchControlRecord = AchBatchControlRecord ByteString
