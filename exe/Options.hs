{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
  ( Options (..)
  , CommonOptions (..)
  , Command (..)
  , ParseOptions (..)
  , getOptions
  )
where

import Options.Applicative qualified as O

data Options = Options
  { common :: CommonOptions
  , command :: Command
  }

data CommonOptions = CommonOptions
  {
  }

data Command
  = Parse ParseOptions

data ParseOptions = ParseOptions
  { file :: FilePath
  }

parseOptions :: O.Parser Options
parseOptions = do
  common <- parseCommonOptions

  command <- parseCommand

  pure Options{..}

parseCommonOptions :: O.Parser CommonOptions
parseCommonOptions = do
  pure CommonOptions{}

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
