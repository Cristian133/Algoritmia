module Parser2 where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import qualified Text.Parsec.Char as C
import Text.Parsec.Language (emptyDef)

import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace cursor
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
cursor :: TokenParser u
cursor = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end",
                                                     "while","do", "repeat"]})

--------------------------------------------------
--- Parser de expresiones de cadenas de caracteres
--------------------------------------------------
strexp :: Parser StrExp
-- strexp  = literal concatopp

strexp  = chainl1 strexp2 (try (do reservedOp cursor "++"
                                   return Concat))
strexp2 = try (parens cursor strexp)
          <|>  (do lit <- stringLiteral cursor
                   return (Lit lit))

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp cursor ";"
                              return Seq))

comm2 = try (do s1 <- identifier cursor
                reservedOp cursor ":="
                s2 <- strexp
                return (Let s1 s2))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
