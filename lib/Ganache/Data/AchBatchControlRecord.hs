module Ganache.Data.AchBatchControlRecord
  ( AchBatchControlRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print

newtype AchBatchControlRecord = AchBatchControlRecord ByteString

instance Parse AchBatchControlRecord where
  parseF :: ParserF AchBatchControlRecord
  parseF = do
    undefined

  parseM :: ParserM AchBatchControlRecord
  parseM = do
    undefined

instance Print AchBatchControlRecord where
  print :: AchBatchControlRecord -> ByteString
  print (AchBatchControlRecord bytes) = Char8.cons '8' bytes
