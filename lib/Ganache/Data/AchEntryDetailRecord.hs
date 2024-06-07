{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchEntryDetailRecord
  ( AchEntryDetailRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal (c2w)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString
  deriving stock (Show, Eq)

instance FromAch AchEntryDetailRecord where
  parseAch :: Parser AchEntryDetailRecord
  parseAch = do
    b <- M.char (c2w '6')
    bs <- M.takeP Nothing 93
    pure $ AchEntryDetailRecord (b `ByteString.cons` bs)

instance ToAch AchEntryDetailRecord where
  toAch :: AchEntryDetailRecord -> ByteString
  toAch (AchEntryDetailRecord bytes) = bytes
