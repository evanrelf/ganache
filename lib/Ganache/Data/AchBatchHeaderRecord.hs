{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchBatchHeaderRecord = AchBatchHeaderRecord
  { recordTypeCode :: !Word8
  , serviceClassCode :: !ByteString
  , companyName :: !ByteString
  , companyDiscretionaryData :: !ByteString
  , companyIdentification :: !ByteString
  , standardEntryClassCode :: !ByteString
  , companyEntryDescription :: !ByteString
  , companyDescriptiveDate :: !ByteString
  , effectiveEntryDate :: !ByteString
  , settlementDate :: !ByteString
  , originatorStatusCode :: !ByteString
  , originatingDfiIdentification :: !ByteString
  , batchNumber :: !ByteString
  }
  deriving stock (Show, Eq)

instance FromAch AchBatchHeaderRecord where
  parseAch :: Parser AchBatchHeaderRecord
  parseAch = do
    recordTypeCode <- M.char (c2w '5')
    serviceClassCode <- M.takeP Nothing 3
    companyName <- M.takeP Nothing 16
    companyDiscretionaryData <- M.takeP Nothing 20
    companyIdentification <- M.takeP Nothing 10
    standardEntryClassCode <- M.takeP Nothing 3
    companyEntryDescription <- M.takeP Nothing 10
    companyDescriptiveDate <- M.takeP Nothing 6
    effectiveEntryDate <- M.takeP Nothing 6
    settlementDate <- M.takeP Nothing 3
    originatorStatusCode <- M.takeP Nothing 1
    originatingDfiIdentification <- M.takeP Nothing 8
    batchNumber <- M.takeP Nothing 7
    pure AchBatchHeaderRecord{..}

instance ToAch AchBatchHeaderRecord where
  toAch :: AchBatchHeaderRecord -> ByteString
  toAch x =
    mconcat
      [ ByteString.singleton x.recordTypeCode
      , x.serviceClassCode
      , x.companyName
      , x.companyDiscretionaryData
      , x.companyIdentification
      , x.standardEntryClassCode
      , x.companyEntryDescription
      , x.companyDescriptiveDate
      , x.effectiveEntryDate
      , x.settlementDate
      , x.originatorStatusCode
      , x.originatingDfiIdentification
      , x.batchNumber
      ]
