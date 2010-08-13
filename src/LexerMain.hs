module Main where
import System ( getArgs )
import Lexer

main :: IO()
main = do args <- getArgs;
          contents <- readFile (head (args))
          print (lexer contents)

