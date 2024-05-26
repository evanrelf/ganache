module Ganache.Data.AchFileControlRecord
  ( AchFileControlRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print

newtype AchFileControlRecord = AchFileControlRecord ByteString

instance Parse AchFileControlRecord where
  parseF :: ParserF AchFileControlRecord
  parseF = do
    undefined

  parseM :: ParserM AchFileControlRecord
  parseM = do
    undefined

instance Print AchFileControlRecord where
  print :: AchFileControlRecord -> ByteString
  print (AchFileControlRecord bytes) = Char8.cons '9' bytes
