module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones de Cadenas de caracteres
data StrExp = Lit String
            | Var Variable
            | Concat StrExp StrExp
 deriving (Show,Eq)

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Name Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
--          | Question BoolExp IntExp IntExp -- a > 0 ? 1 : 2;
 deriving (Show,Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Let Variable StrExp
          | Set Variable IntExp
          | Bet Variable BoolExp
          | Seq Comm Comm
 deriving (Show,Eq)
