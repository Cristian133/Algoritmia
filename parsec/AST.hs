module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones de Cadenas de caracteres
data StrExp = Lit String
            | Var Variable
            | Concat StrExp StrExp
 deriving (Show,Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Let Variable StrExp
          | Seq Comm Comm
 deriving (Show,Eq)
