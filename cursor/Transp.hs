module Transp (transp) where

import Data.List

import AST

 -- Remove punctuation from text String.
removeInutilChar :: String -> String
removeInutilChar xs = [x | x <- xs, not (x `elem` "\"\\")]

-- transpua un comando
transp :: Comm -> String

transp (Iet var expInt)   = let v = show (transpIntExp expInt)
                            in "let " ++ var ++ " = " ++ v ++ " "
transp (Set var expStr)   = let v = transpStrExp expStr
                            in "let " ++ var ++ " = " ++ v ++ " "
transp (Bet var expBool)  = let v = show (transpBoolExp expBool)
                            in "let " ++ var ++ " = " ++ v ++ " "

transp Skip = []
transp (Seq Skip c)       = transp c
transp (Seq c1 c2)        = let s1 = show (transp (Seq Skip c1))
                                s2 = show (transp (Seq Skip c2))
                            in  removeInutilChar s1 ++ removeInutilChar s2

transp (Cond b c1 c2)     = let b' = show (transpBoolExp b)
                                s1 =  show (transp c1)
                                s2 =  show (transp c2)
                            in "if " ++ b' ++ " " ++ s1 ++ " else " ++ s2 ++ " endif "

transp (Repeat c b)       = let b' = show (transpBoolExp b)
                                s  = show (transp c)
                            in "while " ++ b' ++ " " ++ s ++ " endwhile "

-- transpua una expresion entera
transpIntExp :: IntExp -> String
transpIntExp (Const valor)        = show (valor)
transpIntExp (UMinus expInt)      = let valor = show (transpIntExp expInt)
                                    in " -" ++ valor
transpIntExp (Plus exp1 exp2)     = let valor1 = show (transpIntExp exp1)
                                        valor2 = show (transpIntExp exp2)
                                    in valor1 ++ " + " ++ valor2

transpIntExp (Minus exp1 exp2)    = let valor1 = show (transpIntExp exp1)
                                        valor2 = show (transpIntExp exp2)
                                    in valor1 ++ " - " ++ valor2
transpIntExp (Times exp1 exp2)    = let valor1 = show (transpIntExp exp1)
                                        valor2 = show (transpIntExp exp2)
                                    in valor1 ++ " * " ++ valor2
transpIntExp (Div exp1 exp2)      = let valor1 = show (transpIntExp exp1)
                                        valor2 = show (transpIntExp exp2)
                                    in valor1 ++ " / " ++ valor2

-- transpua una expresion de cadena
transpStrExp :: StrExp -> String
transpStrExp (Literal valor) = valor
transpStrExp (Concat exp1 exp2) = let valor1 = show (transpStrExp exp1)
                                      valor2 = show (transpStrExp exp2)
                                  in "\'" ++ valor1 ++ "\'" ++ " . " ++  "\'" ++ valor2 ++ "\'"

-- built-in function
transpStrExp CurrentLineStr       = "call getline('.')"
transpStrExp CurrentLineInt       = "call line('.')"
transpStrExp LastLineStr          = "call getline('$')"
transpStrExp LastLineInt          = "call line('$')"
transpStrExp CurrentColInt        = "call col('.')"
transpStrExp (Cursor exp1 exp2)   = let s1 = show (transpIntExp exp1)
                                        s2 = show (transpIntExp exp1)
                                    in "call cursor(" ++ s1 ++ "," ++ s2 ++ ")"
transpStrExp (SubString exp1 exp2)   = let s1 = transpStrExp exp1
                                           s2 = transpStrExp exp2
                                       in "call istridx(" ++ s1 ++ ", " ++ s2 ++ ")"

-- transpua una expresion booleana
transpBoolExp :: BoolExp -> String
transpBoolExp BTrue = "True"
transpBoolExp BFalse = "False"
transpBoolExp (Eq exp1 exp2)  = let valor1 = show (transpIntExp exp1)
                                    valor2 = show (transpIntExp exp2)
                                in valor1 ++ " == " ++ valor2

transpBoolExp (Lt exp1 exp2)  = let valor1 = show (transpIntExp exp1)
                                    valor2 = show (transpIntExp exp2)
                                in valor1 ++ " < " ++ valor2

transpBoolExp (Gt exp1 exp2)  = let valor1 = show (transpIntExp exp1)
                                    valor2 = show (transpIntExp exp2)
                                in valor1 ++ " > " ++ valor2

transpBoolExp (And exp1 exp2) = let valor1 = show (transpBoolExp exp1)
                                    valor2 = show (transpBoolExp exp2)
                                in valor1 ++ " && " ++ valor2

transpBoolExp (Or exp1 exp2)  = let valor1 = show (transpBoolExp exp1)
                                    valor2 = show (transpBoolExp exp2)
                                in valor1 ++ " || " ++ valor2
transpBoolExp (Not exp1)      = "not " ++ (show (transpBoolExp exp1))
