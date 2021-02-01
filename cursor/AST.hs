module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones de Cadenas de caracteres
data StrExp = Literal String
            | Name Variable
            | Concat StrExp StrExp
            | CurrentLineStr
            | CurrentLineInt
            | LastLineStr
            | LastLineInt
            | CurrentColInt
            | Cursor IntExp IntExp
            | SubString StrExp StrExp
            | AvanzarLinea IntExp
            | Reemplazar StrExp StrExp
 deriving (Show,Eq)

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Var Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
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
 deriving (Show,Eq)
