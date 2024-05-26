{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ganache.Parse
  ( achFileF
  , achBatchF
  , achRecordF
  , achBatchRecordF
  , achFileHeaderRecordF
  , achBatchHeaderRecordF
  , achEntryDetailRecordF
  , achAddendaRecordF
  , achBatchControlRecordF
  , achFileControlRecordF
  , achFilePaddingRecordF

  , achFileM
  , achBatchM
  , achRecordM
  , achBatchRecordM
  , achFileHeaderRecordM
  , achBatchHeaderRecordM
  , achEntryDetailRecordM
  , achAddendaRecordM
  , achBatchControlRecordM
  , achFileControlRecordM
  , achFilePaddingRecordM
  )
where

import Control.Applicative (Alternative (..), asum)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (c2w)
import Data.Void (Void)
import FlatParse.Basic qualified as F
import Ganache.Data
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

achFileF :: F.Parser () AchFile
achFileF = do
  header <- achFileHeaderRecordF <* $(F.char '\n')
  batches <- F.many achBatchF
  control <- achFileControlRecordF <* $(F.char '\n')
  padding <- length <$> F.many (achFilePaddingRecordF <* $(F.char '\n'))
  pure AchFile{..}

achFileM :: M.Parsec Void ByteString AchFile
achFileM = do
  header <- achFileHeaderRecordM <* M.newline
  batches <- M.many achBatchM
  control <- achFileControlRecordM <* M.newline
  padding <- length <$> M.many (achFilePaddingRecordM <* M.newline)
  pure AchFile{..}

achBatchF :: F.Parser () AchBatch
achBatchF = do
  header <- achBatchHeaderRecordF <* $(F.char '\n')
  records <- F.many (achBatchRecordF <* $(F.char '\n'))
  control <- achBatchControlRecordF <* $(F.char '\n')
  pure AchBatch{..}

achBatchM :: M.Parsec Void ByteString AchBatch
achBatchM = do
  header <- achBatchHeaderRecordM <* M.newline
  records <- M.many (achBatchRecordM <* M.newline)
  control <- achBatchControlRecordM <* M.newline
  pure AchBatch{..}

achRecordF :: F.Parser () AchRecord
achRecordF = do
  asum
    [ AchRecord_FileHeader <$> achFileHeaderRecordF
    , AchRecord_BatchHeader <$> achBatchHeaderRecordF
    , AchRecord_EntryDetail <$> achEntryDetailRecordF
    , AchRecord_Addenda <$> achAddendaRecordF
    , AchRecord_BatchControl <$> achBatchControlRecordF
    , AchRecord_FileControl <$> achFileControlRecordF
    , AchRecord_FilePadding <$> achFilePaddingRecordF
    ]

achRecordM :: M.Parsec Void ByteString AchRecord
achRecordM = do
  asum
    [ AchRecord_FileHeader <$> achFileHeaderRecordM
    , AchRecord_BatchHeader <$> achBatchHeaderRecordM
    , AchRecord_EntryDetail <$> achEntryDetailRecordM
    , AchRecord_Addenda <$> achAddendaRecordM
    , AchRecord_BatchControl <$> achBatchControlRecordM
    , AchRecord_FileControl <$> achFileControlRecordM
    , AchRecord_FilePadding <$> achFilePaddingRecordM
    ]

achBatchRecordF :: F.Parser () AchBatchRecord
achBatchRecordF = entryDetail <|> addenda
  where
  entryDetail = AchBatchRecord_EntryDetail <$> achEntryDetailRecordF
  addenda = AchBatchRecord_Addenda <$> achAddendaRecordF

achBatchRecordM :: M.Parsec Void ByteString AchBatchRecord
achBatchRecordM = entryDetail <|> addenda
  where
  entryDetail = AchBatchRecord_EntryDetail <$> achEntryDetailRecordM
  addenda = AchBatchRecord_Addenda <$> achAddendaRecordM

achFileHeaderRecordF :: F.Parser () AchFileHeaderRecord
achFileHeaderRecordF = do
  $(F.char '1')
  let recordTypeCode = Char8.singleton '1'
  priorityCode <- F.take 2
  immediateDestination <- F.take 10
  immediateOrigin <- F.take 10
  fileCreationDate <- F.take 6
  fileCreationTime <- F.take 4
  fileIdModifier <- F.take 1
  recordSize <- F.take 3
  blockingFactor <- F.take 2
  formatCode <- F.take 1
  destination <- F.take 23
  originOrCompanyName <- F.take 23
  referenceCode <- F.take 8
  pure AchFileHeaderRecord{..}

achFileHeaderRecordM :: M.Parsec Void ByteString AchFileHeaderRecord
achFileHeaderRecordM = do
  _recordTypeCode <- M.char (c2w '1')
  let recordTypeCode = Char8.singleton '1'
  priorityCode <- M.takeP Nothing 2
  immediateDestination <- M.takeP Nothing 10
  immediateOrigin <- M.takeP Nothing 10
  fileCreationDate <- M.takeP Nothing 6
  fileCreationTime <- M.takeP Nothing 4
  fileIdModifier <- M.takeP Nothing 1
  recordSize <- M.takeP Nothing 3
  blockingFactor <- M.takeP Nothing 2
  formatCode <- M.takeP Nothing 1
  destination <- M.takeP Nothing 23
  originOrCompanyName <- M.takeP Nothing 23
  referenceCode <- M.takeP Nothing 8
  pure AchFileHeaderRecord{..}

achBatchHeaderRecordF :: F.Parser () AchBatchHeaderRecord
achBatchHeaderRecordF = do
  $(F.char '5')
  bytes <- F.take 93
  pure $ AchBatchHeaderRecord bytes

achBatchHeaderRecordM :: M.Parsec Void ByteString AchBatchHeaderRecord
achBatchHeaderRecordM = do
  _ <- M.char (c2w '5')
  bytes <- M.takeP Nothing 93
  pure $ AchBatchHeaderRecord bytes

achEntryDetailRecordF :: F.Parser () AchEntryDetailRecord
achEntryDetailRecordF = do
  $(F.char '6')
  bytes <- F.take 93
  pure $ AchEntryDetailRecord bytes

achEntryDetailRecordM :: M.Parsec Void ByteString AchEntryDetailRecord
achEntryDetailRecordM = do
  _ <- M.char (c2w '6')
  bytes <- M.takeP Nothing 93
  pure $ AchEntryDetailRecord bytes

achAddendaRecordF :: F.Parser () AchAddendaRecord
achAddendaRecordF = do
  $(F.char '7')
  bytes <- F.take 93
  pure $ AchAddendaRecord bytes

achAddendaRecordM :: M.Parsec Void ByteString AchAddendaRecord
achAddendaRecordM = do
  _ <- M.char (c2w '7')
  bytes <- M.takeP Nothing 93
  pure $ AchAddendaRecord bytes

achBatchControlRecordF :: F.Parser () AchBatchControlRecord
achBatchControlRecordF = do
  $(F.char '8')
  bytes <- F.take 93
  pure $ AchBatchControlRecord bytes

achBatchControlRecordM :: M.Parsec Void ByteString AchBatchControlRecord
achBatchControlRecordM = do
  _ <- M.char (c2w '8')
  bytes <- M.takeP Nothing 93
  pure $ AchBatchControlRecord bytes

-- TODO: Currently nothing to disambiguate a file control record from a file
-- padding record

achFileControlRecordF :: F.Parser () AchFileControlRecord
achFileControlRecordF = do
  $(F.char '9')
  bytes <- F.take 93
  pure $ AchFileControlRecord bytes

achFileControlRecordM :: M.Parsec Void ByteString AchFileControlRecord
achFileControlRecordM = do
  _ <- M.char (c2w '9')
  bytes <- M.takeP Nothing 93
  pure $ AchFileControlRecord bytes

achFilePaddingRecordF :: F.Parser () AchFilePaddingRecord
achFilePaddingRecordF = do
  $(F.string (replicate 94 '9'))
  pure AchFilePaddingRecord

achFilePaddingRecordM :: M.Parsec Void ByteString AchFilePaddingRecord
achFilePaddingRecordM = do
  _ <- M.string (Char8.replicate 94 '9')
  pure AchFilePaddingRecord
