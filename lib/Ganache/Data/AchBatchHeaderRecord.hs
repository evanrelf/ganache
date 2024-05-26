{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchBatchHeaderRecord
  ( AchBatchHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (c2w)
import FlatParse.Basic qualified as F
import Ganache.Class.Parse
import Ganache.Class.Print
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

newtype AchBatchHeaderRecord = AchBatchHeaderRecord ByteString

instance Parse AchBatchHeaderRecord where
  parseF :: ParserF AchBatchHeaderRecord
  parseF = do
    $(F.char '5')
    bytes <- F.take 93
    pure $ AchBatchHeaderRecord bytes

  parseM :: ParserM AchBatchHeaderRecord
  parseM = do
    _ <- M.char (c2w '5')
    bytes <- M.takeP Nothing 93
    pure $ AchBatchHeaderRecord bytes

instance Print AchBatchHeaderRecord where
  print :: AchBatchHeaderRecord -> ByteString
  print (AchBatchHeaderRecord bytes) = Char8.cons '5' bytes
