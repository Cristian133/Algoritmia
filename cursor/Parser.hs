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
                                                         "while","do", "repeat",
                                                         "line", "col",                 -- int
                                                         "cantlines", "substr",         -- int
                                                         "cline", "lline",              -- str
                                                         "poscursor", "rplz", "jline"   -- void
                                                        ]})

--------------------------------------------------
--- Parser de expresiones de cadenas de caracteres
--------------------------------------------------
strexp :: Parser StrExp

strexp  = chainl1 strexp2 (try (do reservedOp cursor "++"
                                   return Concat))
strexp2 = try (parens cursor strexp)
          <|> try (do lit <- stringLiteral cursor
                      return (Lit lit))
--          <|>  try (do num <- integer cursor
--                       return (Lit (show num)))
          <|> do str <- reserved cursor "cline"
                 return CurrentLineStr
          <|> do str <- reserved cursor "lline"
                 return LastLineStr
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
                     return (Name str)
                     <|> try (do reserved cursor "line"
                                 return CurrentLineInt)
                     <|> try (do reserved cursor "col"
                                 return CurrentColInt)
                     <|> try (do reserved cursor "cantlines"
                                 return LastLineInt)
                     <|> try (do  reserved cursor "substr"
                                  str1 <- stringLiteral cursor
                                  -- sepBy " "
                                  str2 <- stringLiteral cursor
                                  return (SubString (Lit str1) (Lit str2))))

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

comm2 = try (do reserved cursor "skip"
                return Skip)
        <|> try (do reserved cursor "if"
                    cond <- boolexp
                    reserved cursor "then"
                    case1 <- comm
                    reserved cursor "else"
                    case2 <- comm
                    reserved cursor "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved cursor "repeat"
                    c <- comm
                    reserved cursor "until"
                    cond <- boolexp
                    reserved cursor "end"
                    return (Repeat c cond))
        <|> try (do s1 <- identifier cursor
                    reservedOp cursor "="
                    s2 <- strexp
                    return (Set s1 s2))
        <|> try (do s <- identifier cursor
                    reservedOp cursor "="
                    e <- intexp
                    return (Iet s e))
        <|> try (do s <- identifier cursor
                    reservedOp cursor "="
                    e <- boolexp
                    return (Bet s e))

-----------------------------------
--- Parser de funciones built-in
-----------------------------------
built :: Parser Comm
built = try (do reserved cursor "skip"
                return Skip)
        <|> try (do reserved cursor "if"
                    cond <- boolexp
                    reserved cursor "then"
                    case1 <- comm
                    reserved cursor "else"
                    case2 <- comm
                    reserved cursor "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved cursor "repeat"
                    c <- comm
                    reserved cursor "until"
                    cond <- boolexp
                    reserved cursor "end"
                    return (Repeat c cond))
        <|> try (do s1 <- identifier cursor
                    reservedOp cursor "="
                    s2 <- strexp
                    return (Set s1 s2))
        <|> try (do s <- identifier cursor
                    reservedOp cursor "="
                    e <- intexp
                    return (Iet s e))
        <|> try (do s <- identifier cursor
                    reservedOp cursor "="
                    e <- boolexp
                    return (Bet s e))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totalParser comm)
