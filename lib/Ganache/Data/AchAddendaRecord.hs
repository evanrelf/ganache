module Ganache.Data.AchAddendaRecord
  ( AchAddendaRecord (..)
  )
where

import Data.ByteString (ByteString)

newtype AchAddendaRecord = AchAddendaRecord ByteString
