module Ganache.Data.AchBatchRecord
  ( AchBatchRecord (..)
  )
where

import Ganache.Data.AchAddendaRecord (AchAddendaRecord (..))
import Ganache.Data.AchEntryDetailRecord (AchEntryDetailRecord (..))

data AchBatchRecord
  = EntryDetail AchEntryDetailRecord
  | Addenda AchAddendaRecord
