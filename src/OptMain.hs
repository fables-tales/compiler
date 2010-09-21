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
          let semanticPair = (verifySemantics . parseOptimise . camleParser . lexer) contents;
          if fst semanticPair then putStrLn ((toAssembly . optimiseIr . toIrForm) (snd semanticPair)) else error "bad program semantics"
