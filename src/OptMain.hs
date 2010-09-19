module Main where
import System ( getArgs )

import Parser
import Lexer
import IR
import Semantics
import Optimiser


main :: IO()
main = do args <- getArgs;
		  contents <- readFile (head args);
          let semanticPair = (parseOptimise . verifySemantics . camleParser . lexer) contents;
          if fst semanticPair then putStrLn ((toAssembly . toIrForm) (snd semanticPair)) else error "bad program semantics"
