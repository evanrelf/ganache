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
  , achBatchHeaderRecordF
  , achEntryDetailRecordF
  , achAddendaRecordF
  , achBatchControlRecordF
  , achFileControlRecordF

  , achFileM
  , achBatchM
  , achRecordM
  , achBatchRecordM
  , achBatchHeaderRecordM
  , achEntryDetailRecordM
  , achAddendaRecordM
  , achBatchControlRecordM
  , achFileControlRecordM
  )
where

import Control.Applicative (Alternative (..), asum)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Void (Void)
import FlatParse.Basic qualified as F
import Ganache.Class.Parse
import Ganache.Data
import Ganache.Data.AchBatchRecord qualified as AchBatchRecord
import Ganache.Data.AchRecord qualified as AchRecord
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

achFileF :: F.Parser () AchFile
achFileF = do
  header <- parseF @AchFileHeaderRecord <* $(F.char '\n')
  batches <- F.many achBatchF
  control <- achFileControlRecordF <* $(F.char '\n')
  padding <- length <$> F.many (parseF @AchFilePaddingRecord <* $(F.char '\n'))
  pure AchFile{..}

achFileM :: M.Parsec Void ByteString AchFile
achFileM = do
  header <- parseM @AchFileHeaderRecord <* M.newline
  batches <- M.many achBatchM
  control <- achFileControlRecordM <* M.newline
  padding <- length <$> M.many (parseM @AchFilePaddingRecord <* M.newline)
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
    [ AchRecord.FileHeader <$> parseF @AchFileHeaderRecord
    , AchRecord.BatchHeader <$> achBatchHeaderRecordF
    , AchRecord.EntryDetail <$> achEntryDetailRecordF
    , AchRecord.Addenda <$> achAddendaRecordF
    , AchRecord.BatchControl <$> achBatchControlRecordF
    , AchRecord.FileControl <$> achFileControlRecordF
    , AchRecord.FilePadding <$> parseF @AchFilePaddingRecord
    ]

achRecordM :: M.Parsec Void ByteString AchRecord
achRecordM = do
  asum
    [ AchRecord.FileHeader <$> parseM @AchFileHeaderRecord
    , AchRecord.BatchHeader <$> achBatchHeaderRecordM
    , AchRecord.EntryDetail <$> achEntryDetailRecordM
    , AchRecord.Addenda <$> achAddendaRecordM
    , AchRecord.BatchControl <$> achBatchControlRecordM
    , AchRecord.FileControl <$> achFileControlRecordM
    , AchRecord.FilePadding <$> parseM @AchFilePaddingRecord
    ]

achBatchRecordF :: F.Parser () AchBatchRecord
achBatchRecordF = entryDetail <|> addenda
  where
  entryDetail = AchBatchRecord.EntryDetail <$> achEntryDetailRecordF
  addenda = AchBatchRecord.Addenda <$> achAddendaRecordF

achBatchRecordM :: M.Parsec Void ByteString AchBatchRecord
achBatchRecordM = entryDetail <|> addenda
  where
  entryDetail = AchBatchRecord.EntryDetail <$> achEntryDetailRecordM
  addenda = AchBatchRecord.Addenda <$> achAddendaRecordM

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
