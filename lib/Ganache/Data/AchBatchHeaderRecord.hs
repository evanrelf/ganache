module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString
