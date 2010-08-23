module Main where
import Parser
import Lexer
import IR
import System ( getArgs )


main :: IO()
main = do args <- getArgs;
		  contents <- readFile (head (args));
		  putStrLn ((toAssembly . toIrForm . camleParser . lexer) contents);
