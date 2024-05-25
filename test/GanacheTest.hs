{-# OPTIONS_GHC -Wno-unused-imports #-}

module GanacheTest (module GanacheTest) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Function ((&))
import FlatParse.Basic qualified as FlatParse
import Ganache.Parse qualified as Parse
import Ganache.Print qualified as Print
import Hedgehog hiding (test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.FileSystem.Dir (readFiles)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import Test.Tasty
import Test.Tasty.HUnit

test_roundtripExamples :: IO [TestTree]
test_roundtripExamples =
    readFiles "examples/"
  & fmap (\path -> "examples" </> path)
  & Stream.filter (\path -> "ach" `FilePath.isExtensionOf` path)
  & Stream.mapM (\path -> (path,) <$> ByteString.readFile path)
  & fmap (\(path, bytes) -> testCase path do
      case FlatParse.runParser Parse.achFile bytes of
        FlatParse.Fail -> assertFailure "Parser failure"
        FlatParse.Err () -> assertFailure "Parser error"
        FlatParse.OK achFile rest -> do
          assertEqual "No bytes leftover" ByteString.empty rest
          assertEqual "Roundtrips successfully" bytes (Print.achFile achFile)
    )
  & Stream.toList
