module Parsing 
  ( module Parsing
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , decimal
  , void
  )
  where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (void)
import Data.Void

type Parser = Parsec Void String

parseUnsafe :: Parser a -> String -> a
parseUnsafe p s = case runParser p "" s of
  Left e -> error $ errorBundlePretty e
  Right x -> x
