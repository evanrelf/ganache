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
    Parse parseOptions -> parseMain parseOptions

parseMain :: ParseOptions -> IO ()
parseMain parseOptions = do
  bytes <- readFileBS parseOptions.file

  putTextLn "Parsing with flatparse..."
  achFileF <-
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
  achFileM <-
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

  if achFileF == achFileM then do
    pPrint achFileF
  else do
    putTextLn "warning: flatparse and megaparsec disagree\n"
    putTextLn "What flatparse got:"
    pPrint achFileF
    putTextLn "What megaparsec got:"
    pPrint achFileM
