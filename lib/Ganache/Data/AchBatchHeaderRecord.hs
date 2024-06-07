{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
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

newtype AchBatchHeaderRecord = AchBatchHeaderRecord Text
  deriving stock (Show, Eq)

instance FromAch AchBatchHeaderRecord where
  parseAch :: Parser AchBatchHeaderRecord
  parseAch = do
    _ <- M.char '5'
    text <- M.takeP Nothing 93
    pure $ AchBatchHeaderRecord text

instance ToAch AchBatchHeaderRecord where
  toAch :: AchBatchHeaderRecord -> ByteString
  toAch (AchBatchHeaderRecord text) = Text.encodeUtf8 ('5' `Text.cons` text)
