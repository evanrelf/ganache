module Ganache.Data.AchEntryDetailRecord
  ( AchEntryDetailRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString

instance Parse AchEntryDetailRecord where
  parseF :: ParserF AchEntryDetailRecord
  parseF = do
    undefined

  parseM :: ParserM AchEntryDetailRecord
  parseM = do
    undefined

instance Print AchEntryDetailRecord where
  print :: AchEntryDetailRecord -> ByteString
  print (AchEntryDetailRecord bytes) = Char8.cons '6' bytes
