module Main (main) where

import Data.ByteString qualified as ByteString
import FlatParse.Basic qualified as FlatParse
import Ganache
import Options
import Text.Megaparsec qualified as Megaparsec
import Text.Pretty.Simple qualified as PrettySimple

main :: IO ()
main = do
  options <- getOptions

  case options.command of
    Parse parseOptions -> parseMain options.common parseOptions

parseMain :: CommonOptions -> ParseOptions -> IO ()
parseMain _commonOptions parseOptions = do
  bytes <- ByteString.readFile parseOptions.file

  putTextLn "Parsing with flatparse..."
  flatparseAchFile <-
    case FlatParse.runParser (parseAchF @AchFile) bytes of
      FlatParse.Fail -> die "error: Parser failure"
      FlatParse.Err () -> die "error: Parser error"
      FlatParse.OK achFile rest -> do
        when (rest /= ByteString.empty) do
          putTextLn "warning: Parser did not consume all input"
        putTextLn "Success!"
        pure achFile
  putTextLn ""

  putTextLn "Parsing with megaparsec..."
  megaparsecAchFile <-
    case Megaparsec.runParser (parseAchM @AchFile) parseOptions.file bytes of
      Left err ->
        die ("error: Parser error:\n" <> Megaparsec.errorBundlePretty err)
      Right achFile -> do
        putTextLn "Success!"
        pure achFile
  putTextLn ""

  let pPrint =
        PrettySimple.pPrintOpt
          PrettySimple.NoCheckColorTty
          PrettySimple.defaultOutputOptionsNoColor
            { PrettySimple.outputOptionsStringStyle = PrettySimple.Literal
            }

  if flatparseAchFile == megaparsecAchFile then do
    pPrint flatparseAchFile
  else do
    putTextLn "warning: flatparse and megaparsec disagree\n"
    putTextLn "What flatparse got:"
    pPrint flatparseAchFile
    putTextLn "What megaparsec got:"
    pPrint megaparsecAchFile
