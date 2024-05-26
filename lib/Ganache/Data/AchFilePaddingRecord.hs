{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFilePaddingRecord
  ( AchFilePaddingRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import FlatParse.Basic qualified as F
import Ganache.Class.Parse
import Ganache.Class.Print
import Text.Megaparsec.Byte qualified as M

data AchFilePaddingRecord = AchFilePaddingRecord

instance Parse AchFilePaddingRecord where
  parseF :: ParserF AchFilePaddingRecord
  parseF = do
    $(F.string (replicate 94 '9'))
    pure AchFilePaddingRecord

  parseM :: ParserM AchFilePaddingRecord
  parseM = do
    _ <- M.string (Char8.replicate 94 '9')
    pure AchFilePaddingRecord

instance Print AchFilePaddingRecord where
  print :: AchFilePaddingRecord -> ByteString
  print _ = Char8.replicate 94 '9'
