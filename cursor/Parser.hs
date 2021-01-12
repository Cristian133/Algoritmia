module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import AST

-- Funcion para facilitar el testing del parser.
totalParser :: Parser a -> Parser a
totalParser p = do
                    whiteSpace cursor
                    t <- p
                    eof
                    return t

-- Analizador de Tokens
cursor :: TokenParser u
cursor = makeTokenParser ( emptyDef   { commentStart  = "/*"
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

strexp  = chainl1 strexp2 (try (do reservedOp cursor "++"
                                   return Concat))
strexp2 = try (parens cursor strexp)
          <|>  (do lit <- stringLiteral cursor
                   return (Lit lit))
--          <|>  (do num <- integer cursor
--                   return (Lit (show num)))

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser IntExp
intexp  = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens cursor intexp)
         <|> try (do reservedOp cursor "-"
                     f <- factor
                     return (UMinus f))
         <|> (do n <- integer cursor
                 return (Const n)
              <|> do str <- identifier cursor
                     return (Name str))

multopp = do try (reservedOp cursor "*")
             return Times
          <|> do try (reservedOp cursor "/")
                 return Div

addopp = do try (reservedOp cursor "+")
            return Plus
         <|> do try (reservedOp cursor "-")
                return Minus

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp cursor "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp cursor "&"
                                     return And))

boolexp3 = try (parens cursor boolexp)
           <|> try (do reservedOp cursor "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp cursor "="
                  return Eq)
          <|> try (do reservedOp cursor "<"
                      return Lt)
          <|> try (do reservedOp cursor ">"
                      return Gt)

boolvalue = try (do reserved cursor "true"
                    return BTrue)
            <|> try (do reserved cursor "false"
                        return BFalse)

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
        <|> try (do s <- identifier cursor
                    reservedOp cursor ":="
                    e <- intexp
                    return (Set s e))
        <|> try (do s <- identifier cursor
                    reservedOp cursor ":="
                    e <- boolexp
                    return (Bet s e))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totalParser comm)
