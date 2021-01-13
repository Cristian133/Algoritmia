module Eval (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]
type StateStr = [(Variable,String)]

-- Estado nulo
initState :: State
initState = []
initStateStr :: StateStr
initStateStr = []

-- Busca el valor de una variabl en un estado
lookfor :: Variable -> State -> Integer
lookfor var ((x,y):xs)= if var == x then y
                                    else lookfor var xs
lookforStr :: Variable -> StateStr -> String
lookforStr varS ((x,y):xs)= if varS == x then y
                                    else lookforStr varS xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update var valor [] = [(var,valor)]
update var valor ((x,y):xs) = if var == x then (var,valor):xs
                                          else (x,y): update var valor xs
updateStr :: Variable -> String -> StateStr -> StateStr
updateStr varS valorS [] = [(varS,valorS)]
updateStr varS valorS ((x,y):xs) = if varS == x then (varS,valorS):xs
                                          else (x,y): updateStr varS valorS xs
-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip e = e
evalComm (Iet var expInt) estado = let valor = evalIntExp expInt estado
                                       in update var valor estado
evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c1 c2) s = let s' = evalComm c1 s
                                  in evalComm (Seq Skip c2) s'
-- Evalua una expresion entera
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const valor) estado = valor
evalIntExp (Name variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = let valor = evalIntExp expInt estado
                                    in (-valor)
evalIntExp (Plus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                         valor2 = evalIntExp exp2 estado
                                         in valor1 + valor2

evalIntExp (Minus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                          in valor1 - valor2
evalIntExp (Times exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                          in valor1 * valor2
evalIntExp (Div exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in div valor1 valor2

-- Evalua una expresion cadena
evalStrExp :: StrExp -> StateStr -> String
evalStrExp (Lit valorS) estadoStr = valorS
evalStrExp (Var variable) estadoStr = lookforStr variable estadoStr
evalStrExp (Concat exp1 exp2) estadoStr = let valor1 = evalStrExp exp1 estadoStr
                                              valor2 = evalStrExp exp2 estadoStr
                                          in valor1 ++ valor2

-- Evalua una expresion entera
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue estado = True
evalBoolExp BFalse estado = False
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 == valor2

evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 < valor2

evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 > valor2

evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                        in valor1 && valor2

evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                        in valor1 || valor2
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)
