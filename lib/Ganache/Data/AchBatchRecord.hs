module Ganache.Data.AchBatchRecord
  ( AchBatchRecord (..)
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Ganache.Data.AchAddendaRecord (AchAddendaRecord (..))
import Ganache.Data.AchEntryDetailRecord (AchEntryDetailRecord (..))

data AchBatchRecord
  = EntryDetail !AchEntryDetailRecord
  | Addenda !AchAddendaRecord
  deriving stock (Show, Eq)

instance FromAch AchBatchRecord where
  parseAch :: Parser AchBatchRecord
  parseAch = entryDetail <|> addenda
    where
    entryDetail = EntryDetail <$> parseAch @AchEntryDetailRecord
    addenda = Addenda <$> parseAch @AchAddendaRecord

instance ToAch AchBatchRecord where
  toAch :: AchBatchRecord -> ByteString
  toAch = \case
    EntryDetail x -> toAch @AchEntryDetailRecord x
    Addenda x -> toAch @AchAddendaRecord x
