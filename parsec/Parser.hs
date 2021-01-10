module Parser where

import Text.Parsec.Prim
import Text.Parsec.Token
import Data.Functor.Identity
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Char

import AST

-- rule es otro parser
p            :: (ParsecT String () Identity a) -> String -> Either P.ParseError a
p rule input = parse rule "" input

strexp  :: P.Parser StrExp
strexp  =  try (do op <- concate
                   return op)
          <|> try (do i <- ident
                      return i)
          <|> try (do l <- literal
                      return l)
          <|> do return (Lit "")

literal :: P.Parser StrExp
literal = do char '"'
             spaces
             c <- letter
             cs <- many (alphaNum <|> char '_')
             spaces
             char '"'
             return (Lit (c:cs))

ident  :: P.Parser StrExp
ident  = do c  <- letter
            cs <- many (alphaNum <|> char '_')
            return (Var (c:cs))

concate :: P.Parser StrExp
concate =  do l <- literal
              spaces
              char '+'
              spaces
              l1 <- literal
              return (Concat l l1)

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: P.Parser Comm

comm = try (do str <- ident
               spaces
               char '='
               spaces
               e <- strexp
               return (Let str e))

