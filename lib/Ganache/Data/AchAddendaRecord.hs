{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchAddendaRecord
  ( AchAddendaRecord (..)
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

newtype AchAddendaRecord = AchAddendaRecord ByteString

instance Parse AchAddendaRecord where
  parseF :: ParserF AchAddendaRecord
  parseF = do
    $(F.char '7')
    bytes <- F.take 93
    pure $ AchAddendaRecord bytes

  parseM :: ParserM AchAddendaRecord
  parseM = do
    _ <- M.char (c2w '7')
    bytes <- M.takeP Nothing 93
    pure $ AchAddendaRecord bytes

instance Print AchAddendaRecord where
  print :: AchAddendaRecord -> ByteString
  print (AchAddendaRecord bytes) = Char8.cons '7' bytes
