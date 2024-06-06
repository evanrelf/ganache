module Main (main) where

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

  putTextLn "Parsing..."
  achFile <-
    case Megaparsec.runParser (parseAch @AchFile) parseOptions.file bytes of
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
