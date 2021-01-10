module Identificadores where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
import Data.Functor.Identity
import Text.ParserCombinators.Parsec

import Utiles (p)

ident  :: Parser String
ident  = do c  <- letter
            cs <- many (alphaNum <|> char '_')
            return (c:cs)


identn  :: Parser String
identn  = do spaces
             c  <- letter
             cs <- many (alphaNum <|> char '_')
             spaces
             ds <- many (alphaNum <|> char '_')
             return (c:cs ++ " " ++ ds)

literal :: Parser String
literal = do char '"'
             spaces
             c <- letter
             cs <- many (alphaNum <|> char '_')
             spaces
             char '"'
             return (c:cs)

literaln :: Parser String
literaln = do spaces
              char '"'
              spaces
              c <- letter
              cs <- many (alphaNum <|> char '_')
              s <- letter
              spaces
              char '"'
              return (c:cs)
