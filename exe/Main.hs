module Main (main) where

import Control.Exception.Safe qualified as Exception
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
  text <- either Exception.throwIO pure (decodeUtf8' bytes)

  putTextLn "Parsing..."
  achFile <-
    case Megaparsec.runParser (parseAch @AchFile) parseOptions.file text of
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

  pPrint achFile
