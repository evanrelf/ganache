{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatch
  ( AchBatch (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import FlatParse.Basic qualified as F
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchBatchRecord (AchBatchRecord (..))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchBatch = AchBatch
  { header :: !AchBatchHeaderRecord
  , records :: ![AchBatchRecord]
  , control :: !AchBatchControlRecord
  }

instance FromAch AchBatch where
  parseAchF :: ParserF AchBatch
  parseAchF = do
    header <- parseAchF @AchBatchHeaderRecord <* $(F.char '\n')
    records <- F.many (parseAchF @AchBatchRecord <* $(F.char '\n'))
    control <- parseAchF @AchBatchControlRecord <* $(F.char '\n')
    pure AchBatch{..}

  parseAchM :: ParserM AchBatch
  parseAchM = do
    header <- parseAchM @AchBatchHeaderRecord <* M.newline
    records <- M.many (parseAchM @AchBatchRecord <* M.newline)
    control <- parseAchM @AchBatchControlRecord <* M.newline
    pure AchBatch{..}

instance ToAch AchBatch where
  toAch :: AchBatch -> ByteString
  toAch x =
    Char8.intercalate
      (Char8.singleton '\n')
      [ toAch @AchBatchHeaderRecord x.header
      , Char8.intercalate
          (Char8.singleton '\n')
          (fmap (toAch @AchBatchRecord) x.records)
      , toAch @AchBatchControlRecord x.control
      ]
