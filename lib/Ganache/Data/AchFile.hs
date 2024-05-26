{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFile
  ( AchFile (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import FlatParse.Basic qualified as F
import Ganache.Class.Parse
import Ganache.Class.Print
import Ganache.Data.AchBatch (AchBatch (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))
import Prelude hiding (print)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchFile = AchFile
  { header :: AchFileHeaderRecord
  , batches :: [AchBatch]
  , control :: AchFileControlRecord
  , padding :: Int
  }

instance Parse AchFile where
  parseF :: ParserF AchFile
  parseF = do
    header <- parseF @AchFileHeaderRecord <* $(F.char '\n')
    batches <- F.many (parseF @AchBatch)
    control <- parseF @AchFileControlRecord <* $(F.char '\n')
    padding <- length <$> F.many (parseF @AchFilePaddingRecord <* $(F.char '\n'))
    pure AchFile{..}

  parseM :: ParserM AchFile
  parseM = do
    header <- parseM @AchFileHeaderRecord <* M.newline
    batches <- M.many (parseM @AchBatch)
    control <- parseM @AchFileControlRecord <* M.newline
    padding <- length <$> M.many (parseM @AchFilePaddingRecord <* M.newline)
    pure AchFile{..}

instance Print AchFile where
  print :: AchFile -> ByteString
  print x =
    Char8.unlines
      [ print @AchFileHeaderRecord x.header
      , Char8.intercalate
          (Char8.singleton '\n')
          (fmap (print @AchBatch) x.batches)
      , print @AchFileControlRecord x.control
      , Char8.intercalate
          (Char8.singleton '\n')
          (replicate x.padding (print AchFilePaddingRecord))
      ]
