module Ganache.Class.FromAch
  ( Parser
  , FromAch (..)
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as M

type Parser = M.Parsec Void Text

class FromAch a where
  parseAch :: Parser a
