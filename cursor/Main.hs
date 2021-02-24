module Main where

import System.Environment (getArgs)
import Parser (parseComm)

-- Modificar este import para usar diferentes evaluadores/transpiladores
import Transp
---------------------------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile =
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right t      -> writeFile "command.vim" (transp t)    --imprimir el resultado en archivo.
     -- Right t    -> print (transp t)                      --imprimir el resultado en pantalla.
     -- Right t    -> print t                               --imprimir el aŕbol en pantalla.

