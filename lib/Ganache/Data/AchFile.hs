module Ganache.Data.AchFile
  ( AchFile (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Ganache.Class.Parse
import Ganache.Class.Print
import Ganache.Data.AchBatch (AchBatch (..))
import Ganache.Data.AchFileControlRecord (AchFileControlRecord (..))
import Ganache.Data.AchFileHeaderRecord (AchFileHeaderRecord (..))
import Ganache.Data.AchFilePaddingRecord (AchFilePaddingRecord (..))
import Prelude hiding (print)

data AchFile = AchFile
  { header :: AchFileHeaderRecord
  , batches :: [AchBatch]
  , control :: AchFileControlRecord
  , padding :: Int
  }

instance Parse AchFile where
  parseF :: ParserF AchFile
  parseF = do
    undefined

  parseM :: ParserM AchFile
  parseM = do
    undefined

instance Print AchFile where
  print :: AchFile -> ByteString
  print x =
    Char8.unlines
      [ print @AchFileHeaderRecord x.header
      , Char8.intercalate
          (Char8.singleton '\n')
          (fmap (print @AchBatch) x.batches)
      , print @AchFileControlRecord x.control
      , Char8.intercalate
          (Char8.singleton '\n')
          (replicate x.padding (print AchFilePaddingRecord))
      ]
