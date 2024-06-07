{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFilePaddingRecord
  ( AchFilePaddingRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as Text
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec.Byte qualified as M

data AchFilePaddingRecord = AchFilePaddingRecord
  deriving stock (Show, Eq)

instance FromAch AchFilePaddingRecord where
  parseAch :: Parser AchFilePaddingRecord
  parseAch = do
    _ <- M.string (Text.replicate 94 "9")
    pure AchFilePaddingRecord

instance ToAch AchFilePaddingRecord where
  toAch :: AchFilePaddingRecord -> ByteString
  toAch _ = Char8.replicate 94 '9'
