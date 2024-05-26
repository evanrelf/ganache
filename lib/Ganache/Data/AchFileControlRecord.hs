{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileControlRecord
  ( AchFileControlRecord (..)
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

newtype AchFileControlRecord = AchFileControlRecord ByteString

instance Parse AchFileControlRecord where
  parseF :: ParserF AchFileControlRecord
  parseF = do
    $(F.char '9')
    bytes <- F.take 93
    pure $ AchFileControlRecord bytes

  parseM :: ParserM AchFileControlRecord
  parseM = do
    _ <- M.char (c2w '9')
    bytes <- M.takeP Nothing 93
    pure $ AchFileControlRecord bytes

instance Print AchFileControlRecord where
  print :: AchFileControlRecord -> ByteString
  print (AchFileControlRecord bytes) = Char8.cons '9' bytes
