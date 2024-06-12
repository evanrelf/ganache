{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileHeaderRecord
  ( AchFileHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal (c2w)
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchFileHeaderRecord = AchFileHeaderRecord
  { recordTypeCode :: !Word8
  , priorityCode :: !ByteString
  , immediateDestination :: !ByteString
  , immediateOrigin :: !ByteString
  , fileCreationDate :: !ByteString
  , fileCreationTime :: !ByteString
  , fileIdModifier :: !Word8
  , recordSize :: !ByteString
  , blockingFactor :: !ByteString
  , formatCode :: !Word8
  , destination :: !ByteString
  , originOrCompanyName :: !ByteString
  , referenceCode :: !ByteString
  }
  deriving stock (Show, Eq)

instance FromAch AchFileHeaderRecord where
  parseAch :: Parser AchFileHeaderRecord
  parseAch = do
    recordTypeCode <- M.char (c2w '1')
    priorityCode <- M.string (Text.encodeUtf8 "01")
    immediateDestination <- M.takeP Nothing 10
    immediateOrigin <- M.takeP Nothing 10
    fileCreationDate <- M.takeP Nothing 6
    fileCreationTime <- M.takeP Nothing 4
    fileIdModifier <- M.satisfy (`elem` fmap c2w (['A'..'Z'] <> ['0'..'9']))
    recordSize <- M.string (Text.encodeUtf8 "094")
    blockingFactor <- M.string (Text.encodeUtf8 "10")
    formatCode <- M.char (c2w '1')
    destination <- M.takeP Nothing 23
    originOrCompanyName <- M.takeP Nothing 23
    referenceCode <- M.takeP Nothing 8
    pure AchFileHeaderRecord{..}

instance ToAch AchFileHeaderRecord where
  toAch :: AchFileHeaderRecord -> ByteString
  toAch x =
    mconcat
      [ ByteString.singleton x.recordTypeCode
      , x.priorityCode
      , x.immediateDestination
      , x.immediateOrigin
      , x.fileCreationDate
      , x.fileCreationTime
      , ByteString.singleton x.fileIdModifier
      , x.recordSize
      , x.blockingFactor
      , ByteString.singleton x.formatCode
      , x.destination
      , x.originOrCompanyName
      , x.referenceCode
      ]
