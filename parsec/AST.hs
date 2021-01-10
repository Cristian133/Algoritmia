module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritmeticas
data StrExp = Lit String
            | Var Variable
            | Concat StrExp StrExp
 deriving (Show,Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Let StrExp StrExp
 deriving (Show,Eq)
