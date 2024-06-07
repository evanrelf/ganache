{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal (c2w)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString
  deriving stock (Show, Eq)

instance FromAch AchBatchHeaderRecord where
  parseAch :: Parser AchBatchHeaderRecord
  parseAch = do
    b <- M.char (c2w '5')
    bs <- M.takeP Nothing 93
    pure $ AchBatchHeaderRecord (b `ByteString.cons` bs)

instance ToAch AchBatchHeaderRecord where
  toAch :: AchBatchHeaderRecord -> ByteString
  toAch (AchBatchHeaderRecord bytes) = bytes
