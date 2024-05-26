module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString

instance Parse AchBatchHeaderRecord where
  parseF :: ParserF AchBatchHeaderRecord
  parseF = do
    undefined

  parseM :: ParserM AchBatchHeaderRecord
  parseM = do
    undefined

instance Print AchBatchHeaderRecord where
  print :: AchBatchHeaderRecord -> ByteString
  print (AchBatchHeaderRecord bytes) = Char8.cons '5' bytes
