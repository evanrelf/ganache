module Ganache.Data.AchRecord
  ( AchRecord (..)
  )
where

import Data.ByteString (ByteString)
import Ganache.Class.Parse
import Ganache.Class.Print
import Ganache.Data.AchAddendaRecord (AchAddendaRecord (..))
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchEntryDetailRecord (AchEntryDetailRecord (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))
import Prelude hiding (print)

data AchRecord
  = FileHeader AchFileHeaderRecord
  | BatchHeader AchBatchHeaderRecord
  | EntryDetail AchEntryDetailRecord
  | Addenda AchAddendaRecord
  | BatchControl AchBatchControlRecord
  | FileControl AchFileControlRecord
  | FilePadding AchFilePaddingRecord

instance Parse AchRecord where
  parseF :: ParserF AchRecord
  parseF = do
    undefined

  parseM :: ParserM AchRecord
  parseM = do
    undefined

instance Print AchRecord where
  print :: AchRecord -> ByteString
  print = \case
    FileHeader x -> print @AchFileHeaderRecord x
    BatchHeader x -> print @AchBatchHeaderRecord x
    EntryDetail x -> print @AchEntryDetailRecord x
    Addenda x -> print @AchAddendaRecord x
    BatchControl x -> print @AchBatchControlRecord x
    FileControl x -> print @AchFileControlRecord x
    FilePadding x -> print @AchFilePaddingRecord x
