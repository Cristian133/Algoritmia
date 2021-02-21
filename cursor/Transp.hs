module Transp (transp) where

import Data.List

import AST

-- comandos
transp :: Comm -> String
transp Skip = []
transp (Seq Skip c)       = transp c
transp (Iet var expInt)   = let v = transpIntExp expInt
                            in "let " ++ var ++ " = " ++ v
transp (Set var expStr)   = let v = transpStrExp expStr
                            in "let " ++ var ++ " = " ++ v
transp (Bet var expBool)  = let v = transpBoolExp expBool
                            in "let " ++ var ++ " = " ++ v
transp (Seq c1 c2)        = let s1 = transp (Seq Skip c1)
                                s2 = transp (Seq Skip c2)
                            in  s1 ++ "\n" ++ s2
transp (Cond b c1 c2)     = let b' = transpBoolExp b
                                s1 = transp c1
                                s2 = transp c2
                            in "if " ++ b' ++  "\n" ++ s1 ++  "\n" ++ "else" ++  "\n" ++ s2 ++  "\n" ++ "endif"

transp (Repeat c b)       = let b' = transpBoolExp b
                                c' = transp c
                            in "while " ++ b' ++  "\n" ++ c' ++  "\n" ++ "endwhile"
transp (Echo s)           = let s' = transpStrExp s
                            in "echom " ++ s'
transp AvanzarLinea       = "call cursor( line('.') + 1, 1)"
transp RetroLinea         = "call cursor( line('.') - 1, 1)"
transp Origen             = "call cursor( 1, 1)"
transp (Reemplazar s1 s2) = let s1' = transpStrExp s1
                                s2' = transpStrExp s2
                            in "execute \"s/" ++ s1' ++ "/" ++ s2'++ "/g\""
transp (ExCommand s)      = let s' = transpStrExp s
                            in s'

-- expresiones enteras
transpIntExp :: IntExp -> String
transpIntExp (Const valor)        = show (valor)
transpIntExp (VarInt valor)       = valor
transpIntExp CurrentLineInt       = "line('.')"
transpIntExp LastLineInt          = "line('$')"
transpIntExp (SubString s)        = let s' = transpStrExp s
                                    in "stridx(getline('.'), " ++ s' ++ ")"
transpIntExp (UMinus expInt)      = let valor = transpIntExp expInt
                                    in " -" ++ valor
transpIntExp (Plus exp1 exp2)     = let valor1 = transpIntExp exp1
                                        valor2 = transpIntExp exp2
                                    in valor1 ++ " + " ++ valor2

transpIntExp (Minus exp1 exp2)    = let valor1 = (transpIntExp exp1)
                                        valor2 = (transpIntExp exp2)
                                    in valor1 ++ " - " ++ valor2
transpIntExp (Times exp1 exp2)    = let valor1 = (transpIntExp exp1)
                                        valor2 = (transpIntExp exp2)
                                    in valor1 ++ " * " ++ valor2
transpIntExp (Div exp1 exp2)      = let valor1 = (transpIntExp exp1)
                                        valor2 = (transpIntExp exp2)
                                    in valor1 ++ " / " ++ valor2

-- expresiones de cadena
transpStrExp :: StrExp -> String
transpStrExp (Literal valor)      = "'" ++ valor ++ "'"
transpStrExp (VarStr valor)       = valor
transpStrExp (Concat exp1 exp2)   = let valor1 = transpStrExp exp1
                                        valor2 = transpStrExp exp2
                                    in  valor1 ++ " . " ++  valor2
transpStrExp CurrentLineStr         = "getline('.')"
transpStrExp LastLineStr            = "getline('$')"

-- expresiones booleanas
transpBoolExp :: BoolExp -> String
transpBoolExp BTrue           = "0"
transpBoolExp BFalse          = "1"
transpBoolExp (VarBool valor) = valor
transpBoolExp (Eq exp1 exp2)  = let valor1 = transpIntExp exp1
                                    valor2 = transpIntExp exp2
                                in valor1 ++ " == " ++ valor2

transpBoolExp (Lt exp1 exp2)  = let valor1 = transpIntExp exp1
                                    valor2 = transpIntExp exp2
                                in valor1 ++ " < " ++ valor2

transpBoolExp (Gt exp1 exp2)  = let valor1 = transpIntExp exp1
                                    valor2 = transpIntExp exp2
                                in valor1 ++ " > " ++ valor2

transpBoolExp (And exp1 exp2) = let valor1 = transpBoolExp exp1
                                    valor2 = transpBoolExp exp2
                                in valor1 ++ " && " ++ valor2

transpBoolExp (Or exp1 exp2)  = let valor1 = transpBoolExp exp1
                                    valor2 = transpBoolExp exp2
                                in valor1 ++ " || " ++ valor2
transpBoolExp (Not exp1)      = "not " ++ (transpBoolExp exp1)
