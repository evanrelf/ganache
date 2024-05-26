module Ganache.Class.FromAch
  ( ParserF
  , ParserM
  , FromAch (..)
  )
where

import Data.ByteString (ByteString)
import Data.Void (Void)
import FlatParse.Basic qualified as F
import Text.Megaparsec qualified as M

type ParserF = F.Parser ()

type ParserM = M.Parsec Void ByteString

class FromAch a where
  parseAchF :: ParserF a
  parseAchM :: ParserM a
