module Ganache.Data.AchFileHeaderRecord
  ( AchFileHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)

data AchFileHeaderRecord = AchFileHeaderRecord
  { recordTypeCode :: ByteString
  , priorityCode :: ByteString
  , immediateDestination :: ByteString
  , immediateOrigin :: ByteString
  , fileCreationDate :: ByteString
  , fileCreationTime :: ByteString
  , fileIdModifier :: ByteString
  , recordSize :: ByteString
  , blockingFactor :: ByteString
  , formatCode :: ByteString
  , destination :: ByteString
  , originOrCompanyName :: ByteString
  , referenceCode :: ByteString
  }
