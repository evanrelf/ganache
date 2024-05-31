{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileControlRecord
  ( AchFileControlRecord (..)
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

data AchFileControlRecord = AchFileControlRecord
  { recordTypeCode :: !ByteString
  , batchCount :: !ByteString
  , blockCount :: !ByteString
  , entryAndAddendaCount :: !ByteString
  , entryHash :: !ByteString
  , totalDebit :: !ByteString
  , totalCredit :: !ByteString
  , reserved :: !ByteString
  }
  deriving stock (Show, Eq)

instance FromAch AchFileControlRecord where
  parseAchF :: ParserF AchFileControlRecord
  parseAchF = do
    $(F.char '9')
    let recordTypeCode = Char8.singleton '9'
    batchCount <- F.take 6
    blockCount <- F.take 6
    entryAndAddendaCount <- F.take 8
    entryHash <- F.take 10
    totalDebit <- F.take 12
    totalCredit <- F.take 12
    reserved <- F.take 39
    pure AchFileControlRecord{..}

  parseAchM :: ParserM AchFileControlRecord
  parseAchM = do
    _ <- M.char (c2w '9')
    let recordTypeCode = Char8.singleton '9'
    batchCount <- M.takeP Nothing 6
    blockCount <- M.takeP Nothing 6
    entryAndAddendaCount <- M.takeP Nothing 8
    entryHash <- M.takeP Nothing 10
    totalDebit <- M.takeP Nothing 12
    totalCredit <- M.takeP Nothing 12
    reserved <- M.takeP Nothing 39
    pure AchFileControlRecord{..}

instance ToAch AchFileControlRecord where
  toAch :: AchFileControlRecord -> ByteString
  toAch x =
    mconcat
      [ x.recordTypeCode
      , x.batchCount
      , x.blockCount
      , x.entryAndAddendaCount
      , x.entryHash
      , x.totalDebit
      , x.totalCredit
      , x.reserved
      ]
