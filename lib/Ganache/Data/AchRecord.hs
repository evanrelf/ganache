module Ganache.Data.AchRecord
  ( AchRecord (..)
  )
where

import Control.Applicative (asum)
import Data.ByteString (ByteString)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Ganache.Data.AchAddendaRecord (AchAddendaRecord (..))
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchEntryDetailRecord (AchEntryDetailRecord (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))

data AchRecord
  = FileHeader !AchFileHeaderRecord
  | BatchHeader !AchBatchHeaderRecord
  | EntryDetail !AchEntryDetailRecord
  | Addenda !AchAddendaRecord
  | BatchControl !AchBatchControlRecord
  | FileControl !AchFileControlRecord
  | FilePadding !AchFilePaddingRecord
  deriving stock (Show, Eq)

instance FromAch AchRecord where
  parseAch :: Parser AchRecord
  parseAch =
    asum
      [ FileHeader <$> parseAch @AchFileHeaderRecord
      , BatchHeader <$> parseAch @AchBatchHeaderRecord
      , EntryDetail <$> parseAch @AchEntryDetailRecord
      , Addenda <$> parseAch @AchAddendaRecord
      , BatchControl <$> parseAch @AchBatchControlRecord
      , FileControl <$> parseAch @AchFileControlRecord
      , FilePadding <$> parseAch @AchFilePaddingRecord
      ]

instance ToAch AchRecord where
  toAch :: AchRecord -> ByteString
  toAch = \case
    FileHeader x -> toAch @AchFileHeaderRecord x
    BatchHeader x -> toAch @AchBatchHeaderRecord x
    EntryDetail x -> toAch @AchEntryDetailRecord x
    Addenda x -> toAch @AchAddendaRecord x
    BatchControl x -> toAch @AchBatchControlRecord x
    FileControl x -> toAch @AchFileControlRecord x
    FilePadding x -> toAch @AchFilePaddingRecord x
