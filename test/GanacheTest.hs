module GanacheTest (module GanacheTest) where

import Control.Exception.Safe (MonadCatch)
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import FlatParse.Basic qualified as FlatParse
import Ganache
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.FileSystem.Dir qualified as Streamly
import System.FilePath qualified as FilePath
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit
import Text.Megaparsec qualified as Megaparsec

test_roundtripExamples :: IO [TestTree]
test_roundtripExamples =
    readFilesRecursive "examples/"
  & Stream.filter (\path -> "ach" `FilePath.isExtensionOf` path)
  & Stream.mapM (\path -> (path,) <$> ByteString.readFile path)
  & fmap (\(path, bytes) ->
      ( path
      , testGroup path
          [ testCase "flatparse" do
              achFile <- flatparse @AchFile bytes
              assertEqual "Prints original file" bytes (toAch achFile)
          , testCase "megaparsec" do
              achFile <- megaparsec @AchFile path bytes
              assertEqual "Prints original file" bytes (toAch achFile)
          , testCase "flatparse and megaparsec agree" do
              achFileF <- flatparse @AchFile bytes
              achFileM <- megaparsec @AchFile path bytes
              assertEqual "flatparse and megaparsec agree" achFileF achFileM
          ]
      )
    )
  -- TODO: Fix parsing of `moov` ACH files
  & fmap (\(path, test) ->
      ( path
      , if "moov" `List.isInfixOf` path
          then ignoreTest test
          else test
      )
    )
  & Stream.toList
  & fmap (sortOn fst >>> fmap snd)

readFilesRecursive :: (MonadIO m, MonadCatch m) => FilePath -> Stream m FilePath
readFilesRecursive root =
    Streamly.readEitherPaths root
  & Stream.concatMap (\case
      Left dir -> readFilesRecursive dir
      Right file -> Stream.fromPure file
    )

flatparse :: forall a. FromAch a => ByteString -> IO a
flatparse bytes =
  case FlatParse.runParser (parseAchF @a) bytes of
    FlatParse.Fail -> assertFailure "Parser failure"
    FlatParse.Err () -> assertFailure "Parser error"
    FlatParse.OK x rest -> do
      assertEqual "No bytes leftover" ByteString.empty rest
      pure x

megaparsec :: forall a. FromAch a => FilePath -> ByteString -> IO a
megaparsec path bytes =
  case Megaparsec.runParser (parseAchM @a) path bytes of
    Left err ->
      assertFailure ("Parser error:\n" <> Megaparsec.errorBundlePretty err)
    Right x -> pure x
