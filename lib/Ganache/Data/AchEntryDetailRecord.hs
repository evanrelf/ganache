{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchEntryDetailRecord
  ( AchEntryDetailRecord (..)
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

newtype AchEntryDetailRecord = AchEntryDetailRecord ByteString

instance Parse AchEntryDetailRecord where
  parseF :: ParserF AchEntryDetailRecord
  parseF = do
    $(F.char '6')
    bytes <- F.take 93
    pure $ AchEntryDetailRecord bytes

  parseM :: ParserM AchEntryDetailRecord
  parseM = do
    _ <- M.char (c2w '6')
    bytes <- M.takeP Nothing 93
    pure $ AchEntryDetailRecord bytes

instance Print AchEntryDetailRecord where
  print :: AchEntryDetailRecord -> ByteString
  print (AchEntryDetailRecord bytes) = Char8.cons '6' bytes
