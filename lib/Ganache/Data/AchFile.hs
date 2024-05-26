{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFile
  ( AchFile (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import FlatParse.Basic qualified as F
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Ganache.Data.AchBatch (AchBatch (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))

import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchFile = AchFile
  { header :: AchFileHeaderRecord
  , batches :: [AchBatch]
  , control :: AchFileControlRecord
  , padding :: Int
  }

instance FromAch AchFile where
  parseAchF :: ParserF AchFile
  parseAchF = do
    header <- parseAchF @AchFileHeaderRecord <* $(F.char '\n')
    batches <- F.many (parseAchF @AchBatch)
    control <- parseAchF @AchFileControlRecord <* $(F.char '\n')
    padding <- length <$> F.many (parseAchF @AchFilePaddingRecord <* $(F.char '\n'))
    pure AchFile{..}

  parseAchM :: ParserM AchFile
  parseAchM = do
    header <- parseAchM @AchFileHeaderRecord <* M.newline
    batches <- M.many (parseAchM @AchBatch)
    control <- parseAchM @AchFileControlRecord <* M.newline
    padding <- length <$> M.many (parseAchM @AchFilePaddingRecord <* M.newline)
    pure AchFile{..}

instance ToAch AchFile where
  toAch :: AchFile -> ByteString
  toAch x =
    Char8.unlines
      [ toAch @AchFileHeaderRecord x.header
      , Char8.intercalate
          (Char8.singleton '\n')
          (fmap (toAch @AchBatch) x.batches)
      , toAch @AchFileControlRecord x.control
      , Char8.intercalate
          (Char8.singleton '\n')
          (replicate x.padding (toAch AchFilePaddingRecord))
      ]
