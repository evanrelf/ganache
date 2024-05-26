{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (c2w)
import FlatParse.Basic qualified as F
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString

instance FromAch AchBatchHeaderRecord where
  parseAchF :: ParserF AchBatchHeaderRecord
  parseAchF = do
    $(F.char '5')
    bytes <- F.take 93
    pure $ AchBatchHeaderRecord bytes

  parseAchM :: ParserM AchBatchHeaderRecord
  parseAchM = do
    _ <- M.char (c2w '5')
    bytes <- M.takeP Nothing 93
    pure $ AchBatchHeaderRecord bytes

instance ToAch AchBatchHeaderRecord where
  toAch :: AchBatchHeaderRecord -> ByteString
  toAch (AchBatchHeaderRecord bytes) = Char8.cons '5' bytes
