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
                                      , reservedNames = ["true","false","skip", "echo",
                                                         "if","then","else","endif",
                                                         "while","since","endwhile",
                                                         "curLine", "curCol", "totLines", "subStr", -- exp int
                                                         "getCurrentLine", "getLastLine",           -- exp str
                                                         "fLine", "rLine", "sust", "excom",
                                                         "origen", "goToLine", "final"
                                                        ]})

--------------------------------------------------
--- Parser de expresiones de cadenas de caracteres
--------------------------------------------------
strexp :: Parser StrExp

strexp  = chainl1 strexp2 (try (do reservedOp cursor "++"
                                   return Concat))
strexp2 = try (parens cursor strexp)
          <|> try (do lit <- stringLiteral cursor
                      return (Literal lit))
          <|> do str <- identifier cursor
                 return (VarStr str)
          <|> do str <- reserved cursor "getCurrentLine"
                 return CurrentLineStr
          <|> do str <- reserved cursor "getLastLine"
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
                     return (VarInt str)
              <|> do reserved cursor "curLine"
                     return CurrentLineInt
              <|> do reserved cursor "curCol"
                     return CurrentColInt
              <|> do reserved cursor "totLines"
                     return LastLineInt
              <|> do reserved cursor "subStr"
                     str <- stringLiteral cursor
                     return (SubString (Literal str)))

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
            <|> try (do str <- identifier cursor
                        return (VarBool str))
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
                    reserved cursor "endif"
                    return (Cond cond case1 case2))
        <|> try (do reserved cursor "while"
                    cond <- boolexp
                    reserved cursor "since"
                    c <- comm
                    reserved cursor "endwhile"
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
        <|> try (do reserved cursor "echo"
                    s <- strexp
                    return (Echo s))
        <|> try (do reserved cursor "fLine"
                    return AvanzarLinea)
        <|> try (do reserved cursor "rLine"
                    return RetroLinea)
        <|> try (do reserved cursor "origen"
                    return Origen)
        <|> try (do reserved cursor "goToLine"
                    e <- intexp
                    return (GoToLine e))
        <|> try (do reserved cursor "final"
                    return Final)
        <|> try (do reserved cursor "sust"
                    s1 <- strexp
                    s2 <- strexp
                    return (Reemplazar s1 s2))
        <|> try (do reserved cursor "excom"
                    s <- strexp
                    return (ExCommand s))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totalParser comm)
