module Ganache.Data.AchBatchRecord
  ( AchBatchRecord (..)
  )
where

import Data.ByteString (ByteString)
import Ganache.Class.Parse
import Ganache.Class.Print
import Ganache.Data.AchAddendaRecord (AchAddendaRecord (..))
import Ganache.Data.AchEntryDetailRecord (AchEntryDetailRecord (..))
import Prelude hiding (print)

data AchBatchRecord
  = EntryDetail AchEntryDetailRecord
  | Addenda AchAddendaRecord

instance Parse AchBatchRecord where
  parseF :: ParserF AchBatchRecord
  parseF = do
    undefined

  parseM :: ParserM AchBatchRecord
  parseM = do
    undefined

instance Print AchBatchRecord where
  print :: AchBatchRecord -> ByteString
  print = \case
    EntryDetail x -> print @AchEntryDetailRecord x
    Addenda x -> print @AchAddendaRecord x
