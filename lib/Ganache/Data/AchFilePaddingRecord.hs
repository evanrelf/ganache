{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFilePaddingRecord
  ( AchFilePaddingRecord (..)
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec.Byte qualified as M

data AchFilePaddingRecord = AchFilePaddingRecord
  deriving stock (Show, Eq)

instance FromAch AchFilePaddingRecord where
  parseAch :: Parser AchFilePaddingRecord
  parseAch = do
    _ <- M.string (Text.replicate 94 "9")
    pure AchFilePaddingRecord

instance ToAch AchFilePaddingRecord where
  toAch :: AchFilePaddingRecord -> Text
  toAch _ = Text.replicate 94 "9"
