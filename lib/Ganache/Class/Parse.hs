module Ganache.Class.Parse
  ( ParserF
  , ParserM
  , Parse (..)
  )
where

import Data.ByteString (ByteString)
import Data.Void (Void)
import FlatParse.Basic qualified as F
import Text.Megaparsec qualified as M

type ParserF = F.Parser ()

type ParserM = M.Parsec Void ByteString

class Parse a where
  parseF :: ParserF a
  parseM :: ParserM a
