module Main where
import Parser
import Lexer
import IR
import Semantics
import System ( getArgs )


main :: IO()
main = do args <- getArgs;
		  contents <- readFile (head (args));
          let semanticPair = (verifySemantics . camleParser . lexer) contents;
          if fst semanticPair == True then putStrLn ((toAssembly . toIrForm) (snd semanticPair)) else error "bad program semantics"
