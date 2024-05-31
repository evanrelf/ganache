{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchEntryDetailRecord
  ( AchEntryDetailRecord (..)
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

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString
  deriving stock (Show, Eq)

instance FromAch AchEntryDetailRecord where
  parseAchF :: ParserF AchEntryDetailRecord
  parseAchF = do
    $(F.char '6')
    bytes <- F.take 93
    pure $ AchEntryDetailRecord bytes

  parseAchM :: ParserM AchEntryDetailRecord
  parseAchM = do
    _ <- M.char (c2w '6')
    bytes <- M.takeP Nothing 93
    pure $ AchEntryDetailRecord bytes

instance ToAch AchEntryDetailRecord where
  toAch :: AchEntryDetailRecord -> ByteString
  toAch (AchEntryDetailRecord bytes) = Char8.cons '6' bytes
