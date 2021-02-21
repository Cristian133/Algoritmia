module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones de Cadenas de caracteres
data StrExp = Literal String
            | VarStr Variable
            | Concat StrExp StrExp
            | CurrentLineStr
            | LastLineStr
 deriving (Show,Eq)

-- Expresiones Aritméticas
data IntExp = Const Integer
            | VarInt Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | CurrentLineInt
            | CurrentColInt
            | LastLineInt
            | SubString StrExp
 deriving (Show,Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | VarBool Variable
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
          | AvanzarLinea
          | RetroLinea
          | Origen
          | GoToLine IntExp
          | DelLine IntExp
          | DelCurLine
          | Final
          | Reemplazar StrExp StrExp
          | AddStrCurPosition StrExp
          | AddStrBeginLine  StrExp
          | AddStrFinalLine  StrExp
          | ExCommand StrExp
          | Echo StrExp
 deriving (Show,Eq)
