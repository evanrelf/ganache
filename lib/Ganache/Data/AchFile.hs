{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFile
  ( AchFile (..)
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Ganache.Data.AchBatch (AchBatch (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

-- TODO: Limit number of padding records to match spec
-- TODO: Handle weird line endings

data AchFile = AchFile
  { header :: !AchFileHeaderRecord
  , batches :: ![AchBatch]
  , control :: !AchFileControlRecord
  , padding :: !Int
  , newlines :: !Int
  }
  deriving stock (Show, Eq)

instance FromAch AchFile where
  parseAch :: Parser AchFile
  parseAch = do
    header <- parseAch @AchFileHeaderRecord <* M.newline
    batches <- M.many (parseAch @AchBatch)
    control <- parseAch @AchFileControlRecord
    padding <- length <$>
      M.many (M.try (M.newline *> parseAch @AchFilePaddingRecord))
    newlines <- length <$> M.many M.newline
    M.eof
    pure AchFile{..}

instance ToAch AchFile where
  toAch :: AchFile -> Text
  toAch x =
    mconcat
      [ toAch @AchFileHeaderRecord x.header <> Text.singleton '\n'
      , Text.unlines (fmap (toAch @AchBatch) x.batches)
      , toAch @AchFileControlRecord x.control
      , mconcat
          (replicate x.padding ('\n' `Text.cons` toAch AchFilePaddingRecord))
      , Text.replicate x.newlines "\n"
      ]
