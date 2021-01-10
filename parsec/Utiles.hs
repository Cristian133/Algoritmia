module Utiles where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
import Data.Functor.Identity
import Text.ParserCombinators.Parsec

-- rule es otro parser
p            :: (ParsecT String () Identity a) -> String -> Either ParseError a
p rule input = parse rule "" input

strexp :: Parser StrExp
srtexp = try (parent)
