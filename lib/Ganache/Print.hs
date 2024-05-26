{-# LANGUAGE NoOverloadedStrings #-}

module Ganache.Print
  ( achFile
  , achBatch
  , achRecord
  , achBatchRecord
  , achFileHeaderRecord
  , achBatchHeaderRecord
  , achEntryDetailRecord
  , achAddendaRecord
  , achBatchControlRecord
  , achFileControlRecord
  , achFilePaddingRecord
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Data
import Ganache.Data.AchBatchRecord qualified as AchBatchRecord
import Ganache.Data.AchRecord qualified as AchRecord

achFile :: AchFile -> ByteString
achFile x =
  Char8.unlines
    [ achFileHeaderRecord x.header
    , Char8.intercalate (Char8.singleton '\n') (fmap achBatch x.batches)
    , achFileControlRecord x.control
    , Char8.intercalate
        (Char8.singleton '\n')
        (replicate x.padding (achFilePaddingRecord AchFilePaddingRecord))
    ]

achBatch :: AchBatch -> ByteString
achBatch x =
  Char8.intercalate
    (Char8.singleton '\n')
    [ achBatchHeaderRecord x.header
    , Char8.intercalate (Char8.singleton '\n') (fmap achBatchRecord x.records)
    , achBatchControlRecord x.control
    ]

achRecord :: AchRecord -> ByteString
achRecord = \case
  AchRecord.FileHeader x -> achFileHeaderRecord x
  AchRecord.BatchHeader x -> achBatchHeaderRecord x
  AchRecord.EntryDetail x -> achEntryDetailRecord x
  AchRecord.Addenda x -> achAddendaRecord x
  AchRecord.BatchControl x -> achBatchControlRecord x
  AchRecord.FileControl x -> achFileControlRecord x
  AchRecord.FilePadding x -> achFilePaddingRecord x

achBatchRecord :: AchBatchRecord -> ByteString
achBatchRecord = \case
  AchBatchRecord.EntryDetail x -> achEntryDetailRecord x
  AchBatchRecord.Addenda x -> achAddendaRecord x

achFileHeaderRecord :: AchFileHeaderRecord -> ByteString
achFileHeaderRecord x =
  mconcat
    [ x.recordTypeCode
    , x.priorityCode
    , x.immediateDestination
    , x.immediateOrigin
    , x.fileCreationDate
    , x.fileCreationTime
    , x.fileIdModifier
    , x.recordSize
    , x.blockingFactor
    , x.formatCode
    , x.destination
    , x.originOrCompanyName
    , x.referenceCode
    ]

achBatchHeaderRecord :: AchBatchHeaderRecord -> ByteString
achBatchHeaderRecord (AchBatchHeaderRecord bytes) = Char8.cons '5' bytes

achEntryDetailRecord :: AchEntryDetailRecord -> ByteString
achEntryDetailRecord (AchEntryDetailRecord bytes) = Char8.cons '6' bytes

achAddendaRecord :: AchAddendaRecord -> ByteString
achAddendaRecord (AchAddendaRecord bytes) = Char8.cons '7' bytes

achBatchControlRecord :: AchBatchControlRecord -> ByteString
achBatchControlRecord (AchBatchControlRecord bytes) = Char8.cons '8' bytes

achFileControlRecord :: AchFileControlRecord -> ByteString
achFileControlRecord (AchFileControlRecord bytes) = Char8.cons '9' bytes

achFilePaddingRecord :: AchFilePaddingRecord -> ByteString
achFilePaddingRecord _ = Char8.replicate 94 '9'
