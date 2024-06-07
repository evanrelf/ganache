{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ganache.Data.AchFileHeaderRecord
  ( AchFileHeaderRecord (..)
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

data AchFileHeaderRecord = AchFileHeaderRecord
  { recordTypeCode :: !Text
  , priorityCode :: !Text
  , immediateDestination :: !Text
  , immediateOrigin :: !Text
  , fileCreationDate :: !Text
  , fileCreationTime :: !Text
  , fileIdModifier :: !Text
  , recordSize :: !Text
  , blockingFactor :: !Text
  , formatCode :: !Text
  , destination :: !Text
  , originOrCompanyName :: !Text
  , referenceCode :: !Text
  }
  deriving stock (Show, Eq)

instance FromAch AchFileHeaderRecord where
  parseAch :: Parser AchFileHeaderRecord
  parseAch = do
    _ <- M.char '1'
    let recordTypeCode = Text.singleton '1'
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
    Text.encodeUtf8
      (mconcat
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
        ])
