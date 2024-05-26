module Ganache.Data.AchRecord
  ( AchRecord (..)
  )
where

import Ganache.Data.AchAddendaRecord (AchAddendaRecord (..))
import Ganache.Data.AchBatchControlRecord (AchBatchControlRecord (..))
import Ganache.Data.AchBatchHeaderRecord (AchBatchHeaderRecord (..))
import Ganache.Data.AchEntryDetailRecord (AchEntryDetailRecord (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))

data AchRecord
  = FileHeader AchFileHeaderRecord
  | BatchHeader AchBatchHeaderRecord
  | EntryDetail AchEntryDetailRecord
  | Addenda AchAddendaRecord
  | BatchControl AchBatchControlRecord
  | FileControl AchFileControlRecord
  | FilePadding AchFilePaddingRecord
