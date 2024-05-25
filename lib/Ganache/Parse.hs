{-# LANGUAGE TemplateHaskell #-}

module Ganache.Parse
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

import Control.Applicative (Alternative (..), asum)
import FlatParse.Basic qualified as F
import Ganache.Data

achFile :: F.Parser () AchFile
achFile = do
  header <- achFileHeaderRecord <* $(F.char '\n')
  batches <- F.many achBatch
  control <- achFileControlRecord <* $(F.char '\n')
  padding <- F.many (achFilePaddingRecord <* $(F.char '\n'))
  pure
    AchFile
      { achFileHeader = header
      , achFileBatches = batches
      , achFileControl = control
      , achFilePadding = length padding
      }

achBatch :: F.Parser () AchBatch
achBatch = do
  header <- achBatchHeaderRecord <* $(F.char '\n')
  records <- F.many (achBatchRecord <* $(F.char '\n'))
  control <- achBatchControlRecord <* $(F.char '\n')
  pure
    AchBatch
      { achBatchHeader = header
      , achBatchRecords = records
      , achBatchControl = control
      }

achRecord :: F.Parser () AchRecord
achRecord = do
  asum
    [ AchRecord_FileHeader <$> achFileHeaderRecord
    , AchRecord_BatchHeader <$> achBatchHeaderRecord
    , AchRecord_EntryDetail <$> achEntryDetailRecord
    , AchRecord_Addenda <$> achAddendaRecord
    , AchRecord_BatchControl <$> achBatchControlRecord
    , AchRecord_FileControl <$> achFileControlRecord
    , AchRecord_FilePadding <$> achFilePaddingRecord
    ]

achBatchRecord :: F.Parser () AchBatchRecord
achBatchRecord = entryDetail <|> addenda
  where
  entryDetail = AchBatchRecord_EntryDetail <$> achEntryDetailRecord
  addenda = AchBatchRecord_Addenda <$> achAddendaRecord

achFileHeaderRecord :: F.Parser () AchFileHeaderRecord
achFileHeaderRecord = do
  $(F.char '1')
  bytes <- F.take 93
  pure $ AchFileHeaderRecord bytes

achBatchHeaderRecord :: F.Parser () AchBatchHeaderRecord
achBatchHeaderRecord = do
  $(F.char '5')
  bytes <- F.take 93
  pure $ AchBatchHeaderRecord bytes

achEntryDetailRecord :: F.Parser () AchEntryDetailRecord
achEntryDetailRecord = do
  $(F.char '6')
  bytes <- F.take 93
  pure $ AchEntryDetailRecord bytes

achAddendaRecord :: F.Parser () AchAddendaRecord
achAddendaRecord = do
  $(F.char '7')
  bytes <- F.take 93
  pure $ AchAddendaRecord bytes

achBatchControlRecord :: F.Parser () AchBatchControlRecord
achBatchControlRecord = do
  $(F.char '8')
  bytes <- F.take 93
  pure $ AchBatchControlRecord bytes

-- TODO: Currently nothing to disambiguate a file control record from a file
-- padding record

achFileControlRecord :: F.Parser () AchFileControlRecord
achFileControlRecord = do
  $(F.char '9')
  bytes <- F.take 93
  pure $ AchFileControlRecord bytes

achFilePaddingRecord :: F.Parser () AchFilePaddingRecord
achFilePaddingRecord = do
  $(F.string (replicate 94 '9'))
  pure AchFilePaddingRecord
