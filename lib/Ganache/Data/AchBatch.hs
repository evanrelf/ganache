module Ganache.Data.AchBatch
  ( AchBatch (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchBatchRecord (AchBatchRecord (..))
import Prelude hiding (print)

data AchBatch = AchBatch
  { header :: AchBatchHeaderRecord
  , records :: [AchBatchRecord]
  , control :: AchBatchControlRecord
  }

instance Parse AchBatch where
  parseF :: ParserF AchBatch
  parseF = do
    undefined

  parseM :: ParserM AchBatch
  parseM = do
    undefined

instance Print AchBatch where
  print :: AchBatch -> ByteString
  print x =
    Char8.intercalate
      (Char8.singleton '\n')
      [ print @AchBatchHeaderRecord x.header
      , Char8.intercalate
          (Char8.singleton '\n')
          (fmap (print @AchBatchRecord) x.records)
      , print @AchBatchControlRecord x.control
      ]
