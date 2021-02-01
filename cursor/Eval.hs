module Eval (eval) where

import Data.List

import AST

 -- Remove punctuation from text String.
removeInutilChar :: String -> String
removeInutilChar xs = [x | x <- xs, not (x `elem` "\"\\")]

-- Evalua un comando
-- Completar definicion
eval :: Comm -> String

eval (Iet var expInt)   = let v = show (evalIntExp expInt)
                          in "let " ++ var ++ " = " ++ v ++ " "
eval (Set var expStr)   = let v = evalStrExp expStr
                          in "let " ++ var ++ " = " ++ v ++ " "
eval (Bet var expBool)  = let v = show (evalBoolExp expBool)
                          in "let " ++ var ++ " = " ++ v ++ " "

eval Skip = []
eval (Seq Skip c)       = eval c
eval (Seq c1 c2)        = let s1 = show (eval (Seq Skip c1))
                              s2 = show (eval (Seq Skip c2))
                          in  removeInutilChar s1 ++ removeInutilChar s2

eval (Cond b c1 c2)     = let b' = show (evalBoolExp b)
                              s1 =  show (eval c1)
                              s2 =  show (eval c2)
                          in "if " ++ b' ++ " " ++ s1 ++ " else " ++ s2 ++ " endif "

eval (Repeat c b)       = let b' = show (evalBoolExp b)
                              s  = show (eval c)
                          in "while " ++ b' ++ " " ++ s ++ " endwhile "

-- Evalua una expresion entera
-- Completar definicion
evalIntExp :: IntExp -> Integer
evalIntExp (Const valor)        = valor
evalIntExp (UMinus expInt)      = let valor = evalIntExp expInt
                                  in (-valor)
evalIntExp (Plus exp1 exp2)     = let valor1 = evalIntExp exp1
                                      valor2 = evalIntExp exp2
                                  in valor1 + valor2

evalIntExp (Minus exp1 exp2)    = let valor1 = evalIntExp exp1
                                      valor2 = evalIntExp exp2
                                  in valor1 - valor2
evalIntExp (Times exp1 exp2)    = let valor1 = evalIntExp exp1
                                      valor2 = evalIntExp exp2
                                  in valor1 * valor2
evalIntExp (Div exp1 exp2)      = let valor1 = evalIntExp exp1
                                      valor2 = evalIntExp exp2
                                  in div valor1 valor2

-- Evalua una expresion de cadena
-- Completar definicion
evalStrExp :: StrExp -> String
evalStrExp (Literal valor) = valor
evalStrExp (Concat exp1 exp2) = let valor1 = evalStrExp exp1
                                    valor2 = evalStrExp exp2
                                in valor1 ++ valor2

-- built-in function
evalStrExp CurrentLineStr       = "call getline('.')"
evalStrExp CurrentLineInt       = "call line('.')"
evalStrExp LastLineStr          = "call getline('$')"
evalStrExp LastLineInt          = "call line('$')"
evalStrExp CurrentColInt        = "call col('.')"
evalStrExp (Cursor exp1 exp2)   = let s1 = show (evalIntExp exp1)
                                      s2 = show (evalIntExp exp1)
                                  in "call cursor(" ++ s1 ++ "," ++ s2 ++ ")"
evalStrExp (SubString exp1 exp2)   = let s1 = evalStrExp exp1
                                         s2 = evalStrExp exp2
                                     in "call istridx(" ++ s1 ++ ", " ++ s2 ++ ")"

-- Evalua una expresion booleana
-- Completar definicion
evalBoolExp :: BoolExp -> Bool
evalBoolExp BTrue = True
evalBoolExp BFalse = False
evalBoolExp (Eq exp1 exp2)  = let valor1 = evalIntExp exp1
                                  valor2 = evalIntExp exp2
                              in valor1 == valor2

evalBoolExp (Lt exp1 exp2)  = let valor1 = evalIntExp exp1
                                  valor2 = evalIntExp exp2
                              in valor1 < valor2

evalBoolExp (Gt exp1 exp2)  = let valor1 = evalIntExp exp1
                                  valor2 = evalIntExp exp2
                              in valor1 > valor2

evalBoolExp (And exp1 exp2) = let valor1 = evalBoolExp exp1
                                  valor2 = evalBoolExp exp2
                              in valor1 && valor2

evalBoolExp (Or exp1 exp2)  = let valor1 = evalBoolExp exp1
                                  valor2 = evalBoolExp exp2
                              in valor1 || valor2
evalBoolExp (Not exp1)      = not (evalBoolExp exp1)
