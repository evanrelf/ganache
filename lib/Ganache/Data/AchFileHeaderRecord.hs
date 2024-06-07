{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileHeaderRecord
  ( AchFileHeaderRecord (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (c2w)
import Ganache.Class.FromAch
import Ganache.Class.ToAch
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M

data AchFileHeaderRecord = AchFileHeaderRecord
  { recordTypeCode :: !ByteString
  , priorityCode :: !ByteString
  , immediateDestination :: !ByteString
  , immediateOrigin :: !ByteString
  , fileCreationDate :: !ByteString
  , fileCreationTime :: !ByteString
  , fileIdModifier :: !ByteString
  , recordSize :: !ByteString
  , blockingFactor :: !ByteString
  , formatCode :: !ByteString
  , destination :: !ByteString
  , originOrCompanyName :: !ByteString
  , referenceCode :: !ByteString
  }
  deriving stock (Show, Eq)

instance FromAch AchFileHeaderRecord where
  parseAch :: Parser AchFileHeaderRecord
  parseAch = do
    _ <- M.char (c2w '1')
    let recordTypeCode = Char8.singleton '1'
    priorityCode <- M.takeP Nothing 2
    immediateDestination <- M.takeP Nothing 10
    immediateOrigin <- M.takeP Nothing 10
    fileCreationDate <- M.takeP Nothing 6
    fileCreationTime <- M.takeP Nothing 4
    fileIdModifier <- M.takeP Nothing 1
    recordSize <- M.takeP Nothing 3
    blockingFactor <- M.takeP Nothing 2
    formatCode <- M.takeP Nothing 1
    destination <- M.takeP Nothing 23
    originOrCompanyName <- M.takeP Nothing 23
    referenceCode <- M.takeP Nothing 8
    pure AchFileHeaderRecord{..}

instance ToAch AchFileHeaderRecord where
  toAch :: AchFileHeaderRecord -> ByteString
  toAch x =
    mconcat
      [ x.recordTypeCode
      , x.priorityCode
      , x.immediateDestination
      , x.immediateOrigin
      , x.fileCreationDate
      , x.fileCreationTime
      , x.fileIdModifier
      , x.recordSize
      , x.blockingFactor
      , x.formatCode
      , x.destination
      , x.originOrCompanyName
      , x.referenceCode
      ]
