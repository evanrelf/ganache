module Ganache.Class.FromAch
  ( Parser
  , FromAch (..)
  )
where

import Data.ByteString (ByteString)
import Data.Void (Void)
import Text.Megaparsec qualified as M

type Parser = M.Parsec Void ByteString

class FromAch a where
  parseAch :: Parser a
