{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileControlRecord
  ( AchFileControlRecord (..)
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

newtype AchFileControlRecord = AchFileControlRecord ByteString

instance FromAch AchFileControlRecord where
  parseAchF :: ParserF AchFileControlRecord
  parseAchF = do
    $(F.char '9')
    bytes <- F.take 93
    pure $ AchFileControlRecord bytes

  parseAchM :: ParserM AchFileControlRecord
  parseAchM = do
    _ <- M.char (c2w '9')
    bytes <- M.takeP Nothing 93
    pure $ AchFileControlRecord bytes

instance ToAch AchFileControlRecord where
  toAch :: AchFileControlRecord -> ByteString
  toAch (AchFileControlRecord bytes) = Char8.cons '9' bytes
