{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchAddendaRecord
  ( AchAddendaRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

newtype AchAddendaRecord = AchAddendaRecord Text
  deriving stock (Show, Eq)

instance FromAch AchAddendaRecord where
  parseAch :: Parser AchAddendaRecord
  parseAch = do
    _ <- M.char '7'
    text <- M.takeP Nothing 93
    pure $ AchAddendaRecord text

instance ToAch AchAddendaRecord where
  toAch :: AchAddendaRecord -> ByteString
  toAch (AchAddendaRecord text) = Text.encodeUtf8 ('7' `Text.cons` text)
