module Ganache.Data.AchAddendaRecord
  ( AchAddendaRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print

newtype AchAddendaRecord = AchAddendaRecord ByteString

instance Parse AchAddendaRecord where
  parseF :: ParserF AchAddendaRecord
  parseF = do
    undefined

  parseM :: ParserM AchAddendaRecord
  parseM = do
    undefined

instance Print AchAddendaRecord where
  print :: AchAddendaRecord -> ByteString
  print (AchAddendaRecord bytes) = Char8.cons '7' bytes
