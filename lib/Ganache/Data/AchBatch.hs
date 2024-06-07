{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatch
  ( AchBatch (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchBatchRecord (AchBatchRecord (..))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

-- TODO: Handle weird line endings

data AchBatch = AchBatch
  { header :: !AchBatchHeaderRecord
  , records :: ![AchBatchRecord]
  , control :: !AchBatchControlRecord
  }
  deriving stock (Show, Eq)

instance FromAch AchBatch where
  parseAch :: Parser AchBatch
  parseAch = do
    header <- parseAch @AchBatchHeaderRecord <* M.newline
    records <- parseAch @AchBatchRecord `M.endBy` M.newline
    control <- parseAch @AchBatchControlRecord <* M.newline
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
