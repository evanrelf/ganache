{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFilePaddingRecord
  ( AchFilePaddingRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import FlatParse.Basic qualified as F
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec.Byte qualified as M

data AchFilePaddingRecord = AchFilePaddingRecord

instance FromAch AchFilePaddingRecord where
  parseAchF :: ParserF AchFilePaddingRecord
  parseAchF = do
    $(F.string (replicate 94 '9'))
    pure AchFilePaddingRecord

  parseAchM :: ParserM AchFilePaddingRecord
  parseAchM = do
    _ <- M.string (Char8.replicate 94 '9')
    pure AchFilePaddingRecord

instance ToAch AchFilePaddingRecord where
  toAch :: AchFilePaddingRecord -> ByteString
  toAch _ = Char8.replicate 94 '9'
