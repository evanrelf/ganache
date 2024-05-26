{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatch
  ( AchBatch (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import FlatParse.Basic qualified as F
import Ganache.Class.Parse
import Ganache.Class.Print
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchBatchRecord (AchBatchRecord (..))
import Prelude hiding (print)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchBatch = AchBatch
  { header :: AchBatchHeaderRecord
  , records :: [AchBatchRecord]
  , control :: AchBatchControlRecord
  }

instance Parse AchBatch where
  parseF :: ParserF AchBatch
  parseF = do
    header <- parseF @AchBatchHeaderRecord <* $(F.char '\n')
    records <- F.many (parseF @AchBatchRecord <* $(F.char '\n'))
    control <- parseF @AchBatchControlRecord <* $(F.char '\n')
    pure AchBatch{..}

  parseM :: ParserM AchBatch
  parseM = do
    header <- parseM @AchBatchHeaderRecord <* M.newline
    records <- M.many (parseM @AchBatchRecord <* M.newline)
    control <- parseM @AchBatchControlRecord <* M.newline
    pure AchBatch{..}

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
