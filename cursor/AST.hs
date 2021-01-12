module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones de Cadenas de caracteres
data StrExp = Lit String
            | Var Variable
            | Concat StrExp StrExp
            | CurrentLineStr
            | LastLineStr
 deriving (Show,Eq)

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Name Variable             --   _
            | CurrentLineInt            --  |
            | CurrentColInt             -- <| funciones built-in de consulta
            | LastLineInt               --  |
            | SubString StrExp StrExp   --  |_
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
data Comm = Skip
          | Set Variable StrExp
          | Iet Variable IntExp
          | Bet Variable BoolExp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          | Cursor IntExp IntExp     -- funciones built-in de acción
          | Reemplazar StrExp StrExp
          | SaltarLinea IntExp
 deriving (Show,Eq)
