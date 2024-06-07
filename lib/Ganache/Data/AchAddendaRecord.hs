{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchAddendaRecord
  ( AchAddendaRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
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
    b <- M.char (c2w '7')
    bs <- M.takeP Nothing 93
    pure $ AchAddendaRecord (b `ByteString.cons` bs)

instance ToAch AchAddendaRecord where
  toAch :: AchAddendaRecord -> ByteString
  toAch (AchAddendaRecord bytes) = bytes
