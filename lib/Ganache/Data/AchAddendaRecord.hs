{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchAddendaRecord
  ( AchAddendaRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (c2w)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

newtype AchAddendaRecord = AchAddendaRecord ByteString
  deriving stock (Show, Eq)

instance FromAch AchAddendaRecord where
  parseAch :: Parser AchAddendaRecord
  parseAch = do
    _ <- M.char (c2w '7')
    bytes <- M.takeP Nothing 93
    pure $ AchAddendaRecord bytes

instance ToAch AchAddendaRecord where
  toAch :: AchAddendaRecord -> ByteString
  toAch (AchAddendaRecord bytes) = Char8.cons '7' bytes
