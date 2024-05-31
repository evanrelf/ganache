{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
  ( Options (..)
  , Command (..)
  , ParseOptions (..)
  , getOptions
  )
where

import Options.Applicative qualified as O

data Options = Options
  { command :: Command
  }

data Command
  = Parse ParseOptions

data ParseOptions = ParseOptions
  { file :: FilePath
  }

parseOptions :: O.Parser Options
parseOptions = do
  command <- parseCommand

  pure Options{..}

parseCommand :: O.Parser Command
parseCommand =
  O.hsubparser . mconcat $
    [ O.command "parse" $ O.info (Parse <$> parseParseOptions) $ mconcat
        [ O.progDesc "Parse an ACH file"
        ]
    ]

parseParseOptions :: O.Parser ParseOptions
parseParseOptions = do
  let dashToStdin = \case
        "-" -> "/dev/stdin"
        x -> x

  file <-
    fmap dashToStdin . O.strArgument . mconcat $
      [ O.metavar "FILE"
      , O.help "Path to ACH file"
      , O.value "/dev/stdin"
      , O.showDefault
      ]

  pure ParseOptions{..}

getOptions :: IO Options
getOptions = do
  let parserPrefs = O.prefs mempty
  let parserInfo = O.info (O.helper <*> parseOptions) mempty
  O.customExecParser parserPrefs parserInfo
