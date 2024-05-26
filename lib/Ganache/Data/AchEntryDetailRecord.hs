module Ganache.Data.AchEntryDetailRecord
  ( AchEntryDetailRecord (..)
  )
where

import Data.ByteString (ByteString)

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString
