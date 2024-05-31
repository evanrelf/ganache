{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchControlRecord
  ( AchBatchControlRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (c2w)
import FlatParse.Basic qualified as F
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchBatchControlRecord = AchBatchControlRecord
  { recordTypeCode :: !ByteString
  , serviceClassCode :: !ByteString
  , entryAndAddendaCount :: !ByteString
  , entryHash :: !ByteString
  , totalDebit :: !ByteString
  , totalCredit :: !ByteString
  , companyIdentification :: !ByteString
  , messageAuthenticationCode :: !ByteString
  , reserved :: !ByteString
  , originatingDfiIdentification :: !ByteString
  , batchNumber :: !ByteString
  }
  deriving stock (Show, Eq)

instance FromAch AchBatchControlRecord where
  parseAchF :: ParserF AchBatchControlRecord
  parseAchF = do
    $(F.char '8')
    let recordTypeCode = Char8.singleton '8'
    serviceClassCode <- F.take 3
    entryAndAddendaCount <- F.take 6
    entryHash <- F.take 10
    totalDebit <- F.take 12
    totalCredit <- F.take 12
    companyIdentification <- F.take 10
    messageAuthenticationCode <- F.take 19
    reserved <- F.take 6
    originatingDfiIdentification <- F.take 8
    batchNumber <- F.take 7
    pure AchBatchControlRecord{..}

  parseAchM :: ParserM AchBatchControlRecord
  parseAchM = do
    _ <- M.char (c2w '8')
    let recordTypeCode = Char8.singleton '8'
    serviceClassCode <- M.takeP Nothing 3
    entryAndAddendaCount <- M.takeP Nothing 6
    entryHash <- M.takeP Nothing 10
    totalDebit <- M.takeP Nothing 12
    totalCredit <- M.takeP Nothing 12
    companyIdentification <- M.takeP Nothing 10
    messageAuthenticationCode <- M.takeP Nothing 19
    reserved <- M.takeP Nothing 6
    originatingDfiIdentification <- M.takeP Nothing 8
    batchNumber <- M.takeP Nothing 7
    pure AchBatchControlRecord{..}

instance ToAch AchBatchControlRecord where
  toAch :: AchBatchControlRecord -> ByteString
  toAch x =
    mconcat
      [ x.recordTypeCode
      , x.serviceClassCode
      , x.entryAndAddendaCount
      , x.entryHash
      , x.totalDebit
      , x.totalCredit
      , x.companyIdentification
      , x.messageAuthenticationCode
      , x.reserved
      , x.originatingDfiIdentification
      , x.batchNumber
      ]
