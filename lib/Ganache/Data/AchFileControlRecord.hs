{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileControlRecord
  ( AchFileControlRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

data AchFileControlRecord = AchFileControlRecord
  { recordTypeCode :: !Text
  , batchCount :: !Text
  , blockCount :: !Text
  , entryAndAddendaCount :: !Text
  , entryHash :: !Text
  , totalDebit :: !Text
  , totalCredit :: !Text
  , reserved :: !Text
  }
  deriving stock (Show, Eq)

instance FromAch AchFileControlRecord where
  parseAch :: Parser AchFileControlRecord
  parseAch = do
    _ <- M.char '9'
    let recordTypeCode = Text.singleton '9'
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
    Text.encodeUtf8
      (mconcat
        [ x.recordTypeCode
        , x.batchCount
        , x.blockCount
        , x.entryAndAddendaCount
        , x.entryHash
        , x.totalDebit
        , x.totalCredit
        , x.reserved
        ])
