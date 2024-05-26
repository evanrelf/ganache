module Ganache.Data.AchBatch
  ( AchBatch (..)
  )
where

import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchBatchRecord (AchBatchRecord (..))

data AchBatch = AchBatch
  { header :: AchBatchHeaderRecord
  , records :: [AchBatchRecord]
  , control :: AchBatchControlRecord
  }
