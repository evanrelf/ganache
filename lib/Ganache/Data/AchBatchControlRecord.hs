{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchControlRecord
  ( AchBatchControlRecord (..)
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

newtype AchBatchControlRecord = AchBatchControlRecord ByteString

instance FromAch AchBatchControlRecord where
  parseAchF :: ParserF AchBatchControlRecord
  parseAchF = do
    $(F.char '8')
    bytes <- F.take 93
    pure $ AchBatchControlRecord bytes

  parseAchM :: ParserM AchBatchControlRecord
  parseAchM = do
    _ <- M.char (c2w '8')
    bytes <- M.takeP Nothing 93
    pure $ AchBatchControlRecord bytes

instance ToAch AchBatchControlRecord where
  toAch :: AchBatchControlRecord -> ByteString
  toAch (AchBatchControlRecord bytes) = Char8.cons '8' bytes
