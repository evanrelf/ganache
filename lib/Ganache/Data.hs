{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

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
  { header :: AchFileHeaderRecord
  , batches :: [AchBatch]
  , control :: AchFileControlRecord
  , padding :: Int
  }

data AchBatch = AchBatch
  { header :: AchBatchHeaderRecord
  , records :: [AchBatchRecord]
  , control :: AchBatchControlRecord
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

data AchFileHeaderRecord = AchFileHeaderRecord
  { recordTypeCode :: ByteString
  , priorityCode :: ByteString
  , immediateDestination :: ByteString
  , immediateOrigin :: ByteString
  , fileCreationDate :: ByteString
  , fileCreationTime :: ByteString
  , fileIdModifier :: ByteString
  , recordSize :: ByteString
  , blockingFactor :: ByteString
  , formatCode :: ByteString
  , destination :: ByteString
  , originOrCompanyName :: ByteString
  , referenceCode :: ByteString
  }

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString

newtype AchAddendaRecord = AchAddendaRecord ByteString

newtype AchBatchControlRecord = AchBatchControlRecord ByteString

newtype AchFileControlRecord = AchFileControlRecord ByteString

data AchFilePaddingRecord = AchFilePaddingRecord
