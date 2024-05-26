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

achFile :: AchFile -> ByteString
achFile x =
  Char8.unlines
    [ achFileHeaderRecord x.achFileHeader
    , Char8.intercalate (Char8.singleton '\n') (fmap achBatch x.achFileBatches)
    , achFileControlRecord x.achFileControl
    , Char8.intercalate
        (Char8.singleton '\n')
        (replicate x.achFilePadding (achFilePaddingRecord AchFilePaddingRecord))
    ]

achBatch :: AchBatch -> ByteString
achBatch x =
  Char8.intercalate
    (Char8.singleton '\n')
    [ achBatchHeaderRecord x.achBatchHeader
    , Char8.intercalate
        (Char8.singleton '\n')
        (fmap achBatchRecord x.achBatchRecords)
    , achBatchControlRecord x.achBatchControl
    ]

achRecord :: AchRecord -> ByteString
achRecord = \case
  AchRecord_FileHeader x -> achFileHeaderRecord x
  AchRecord_BatchHeader x -> achBatchHeaderRecord x
  AchRecord_EntryDetail x -> achEntryDetailRecord x
  AchRecord_Addenda x -> achAddendaRecord x
  AchRecord_BatchControl x -> achBatchControlRecord x
  AchRecord_FileControl x -> achFileControlRecord x
  AchRecord_FilePadding x -> achFilePaddingRecord x

achBatchRecord :: AchBatchRecord -> ByteString
achBatchRecord = \case
  AchBatchRecord_EntryDetail x -> achEntryDetailRecord x
  AchBatchRecord_Addenda x -> achAddendaRecord x

achFileHeaderRecord :: AchFileHeaderRecord -> ByteString
achFileHeaderRecord (AchFileHeaderRecord bytes) = Char8.cons '1' bytes

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
