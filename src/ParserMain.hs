module Main where
import System ( getArgs )

import Lexer
import Parser

main :: IO()
main = do args <- getArgs;
          contents <- readFile (head args)
          print ((camleParser . lexer) contents)
