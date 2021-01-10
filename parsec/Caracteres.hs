module Caracteres where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
import Data.Functor.Identity
import Text.ParserCombinators.Parsec

import Utiles (p)

-- parsea un primer caracter dado de una cadena (input)
pChar           :: Char -> String -> Either ParseError Char
pChar c input   =  parse (char c) "" input

-- parsea un primer caracter (cualquiera) de una cadena (input)
uChar           :: String -> Either ParseError Char
uChar input     =  parse (anyChar) "" input

-- parsea una cadena dada al comienzo de una cadena (input)
pCadena         :: String -> String -> Either ParseError String
pCadena s input = parse (string s) "" input

pDigit          :: String -> Either ParseError Char
pDigit input    =  parse (digit) "" input

pLetter         :: String -> Either ParseError Char
pLetter input   =  parse (letter) "" input

pAlphaNum       :: String -> Either ParseError Char
pAlphaNum input =  parse (alphaNum) "" input

pSpace          :: String -> Either ParseError Char
pSpace input    =  parse (space) "" input

pSpaces         :: String -> Either ParseError ()
pSpaces input   =  parse (spaces) "" input

pUnoDe          :: String -> String -> Either ParseError Char
pUnoDe s inp    =  parse (oneOf s) "" inp
