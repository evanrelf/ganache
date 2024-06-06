{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchEntryDetailRecord
  ( AchEntryDetailRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

newtype AchEntryDetailRecord = AchEntryDetailRecord Text
  deriving stock (Show, Eq)

instance FromAch AchEntryDetailRecord where
  parseAch :: Parser AchEntryDetailRecord
  parseAch = do
    _ <- M.char '6'
    text <- M.takeP Nothing 93
    pure $ AchEntryDetailRecord text

instance ToAch AchEntryDetailRecord where
  toAch :: AchEntryDetailRecord -> ByteString
  toAch (AchEntryDetailRecord text) = Text.encodeUtf8 ('6' `Text.cons` text)
