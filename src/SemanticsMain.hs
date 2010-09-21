module Main where
import System ( getArgs )

import Parser
import Lexer
import IR
import Semantics


main :: IO()
main = do args <- getArgs;
          contents <- readFile (head args);
          let semanticPair = (verifySemantics . camleParser . lexer) contents;
          if fst semanticPair then print (snd semanticPair) else error "bad program semantics"

