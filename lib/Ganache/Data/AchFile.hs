module Ganache.Data.AchFile
  ( AchFile (..)
  )
where

import Ganache.Data.AchBatch (AchBatch (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))

data AchFile = AchFile
  { header :: AchFileHeaderRecord
  , batches :: [AchBatch]
  , control :: AchFileControlRecord
  , padding :: Int
  }
