module Ganache.Data
  ( AchFile (..)
  , AchBatch (..)
  , AchRecord (..)
  , AchBatchRecord (..)
  , AchFileHeaderRecord (..)
  , AchBatchHeaderRecord (..)
  , AchEntryDetailRecord (..)
  , AchAddendaRecord (..)
  , AchBatchControlRecord (..)
  , AchFileControlRecord (..)
  , AchFilePaddingRecord (..)
  )
where

import Data.ByteString (ByteString)

data AchFile = AchFile
  { achFileHeader :: AchFileHeaderRecord
  , achFileBatches :: [AchBatch]
  , achFileControl :: AchFileControlRecord
  , achFilePadding :: Int
  }

data AchBatch = AchBatch
  { achBatchHeader :: AchBatchHeaderRecord
  , achBatchRecords :: [AchBatchRecord]
  , achBatchControl :: AchBatchControlRecord
  }

data AchRecord
  = AchRecord_FileHeader AchFileHeaderRecord
  | AchRecord_BatchHeader AchBatchHeaderRecord
  | AchRecord_EntryDetail AchEntryDetailRecord
  | AchRecord_Addenda AchAddendaRecord
  | AchRecord_BatchControl AchBatchControlRecord
  | AchRecord_FileControl AchFileControlRecord
  | AchRecord_FilePadding AchFilePaddingRecord

data AchBatchRecord
  = AchBatchRecord_EntryDetail AchEntryDetailRecord
  | AchBatchRecord_Addenda AchAddendaRecord

newtype AchFileHeaderRecord = AchFileHeaderRecord ByteString

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString

newtype AchAddendaRecord = AchAddendaRecord ByteString

newtype AchBatchControlRecord = AchBatchControlRecord ByteString

newtype AchFileControlRecord = AchFileControlRecord ByteString

data AchFilePaddingRecord = AchFilePaddingRecord
