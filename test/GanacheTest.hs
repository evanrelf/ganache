module GanacheTest (module GanacheTest) where

import Control.Exception.Safe (MonadCatch)
import Ganache
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.FileSystem.Dir qualified as Streamly
import System.FilePath qualified as FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec qualified as Megaparsec

test_roundtripExamples :: IO [TestTree]
test_roundtripExamples =
    readFilesRecursive "examples/"
  & Stream.filter (\path -> "ach" `FilePath.isExtensionOf` path)
  & Stream.mapM (\path -> (path,) <$> readFileBS path)
  & fmap (\(path, bytes) ->
      ( path
      , testCase path do
          achFile <- parse @AchFile path bytes
          assertEqual "Prints original file" bytes (toAch achFile)
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

parse :: forall a. FromAch a => FilePath -> ByteString -> IO a
parse path bytes =
  case Megaparsec.runParser (parseAch @a) path bytes of
    Left err ->
      assertFailure ("Parser error:\n" <> Megaparsec.errorBundlePretty err)
    Right x -> pure x
