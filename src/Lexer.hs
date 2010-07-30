module Lexer where
import Data.List
import Data.Char

isWhiteSpace :: Char -> Bool
isWhiteSpace x = x `elem` "'\n \t\r"

data Token = TokenIdentifier String |
             --separates things
             TokenComma |
             --program start
             TokenProgram |

             --int literal, reals aren't lexed as tokens, but are parsed instead
             TokenIntLiteral Int |

             --type declrations
             TokenINTEGER | TokenREAL |

             -- '.' and 'e' for floating point
             TokenDot | TokenE |

             --operators
             TokenPlus | TokenMinus |
             TokenMul | TokenDiv |

             -- strings
             TokenStringLiteral String |

             -- open and close bracket
             TokenOb | TokenCb |

             -- control flow tokens
             TokenIf | TokenElse |
             TokenSemiColon |
             TokenRepeat | TokenUntil |
             TokenBegin | TokenEnd |

             -- declarations block begins with "VAR" (in any capitalisation)
             TokenVar |

             -- variable := expressions
             TokenAssign |

             --tokens for io commands
             TokenRead |
             TokenWrite | TokenWriteLn |

             --relation tokens
             TokenGreater | TokenGreaterEq |
             TokenEq | TokenNeq |
             TokenLessEq |
             TokenLess deriving (Show, Eq)
--deals with all tokens that are simple strings - nb cast everything to lowercase because we can.
stringTokens :: [(String, Token)]
stringTokens = [
                        ("+", TokenPlus), ("-", TokenMinus),
                        ("*", TokenMul), ("/", TokenDiv),
                        (":=", TokenAssign),
                        ("=", TokenEq), ("!=", TokenNeq),
                        ("<", TokenLess),("<=", TokenLessEq),
                        (">", TokenGreater), (">=", TokenGreaterEq),
                        ("(", TokenOb), (")", TokenCb),
                        ("read", TokenRead),
                        ("write", TokenWrite),
                        ("writeln", TokenWriteLn),
                        (";", TokenSemiColon),
                        ("var", TokenVar),
                        ("if", TokenIf),
                        ("else", TokenElse),
                        ("repeat", TokenRepeat),
                        ("until", TokenUntil),
                        ("end", TokenEnd),
                        ("begin", TokenBegin),
                        ("program", TokenProgram),
                        ("integer", TokenINTEGER), ("real", TokenREAL),
                        (".", TokenDot),
                        (",", TokenComma),
                        ("e", TokenE)
               ]

--extracts a token from a string from the above simple strings map
getTokenForString :: String -> (Maybe Token, Int)
getTokenForString s = let pair = (find ((`isPrefixOf` s) . fst) stringTokens) in
                        if pair == Nothing then (Nothing, 0)
                        else let certainPair = certain pair in
                            (Just (snd certainPair), length (fst certainPair))

--define the quote character magic constants all over the place suck
stringDelimiter :: Char
stringDelimiter = '\''

--turn a maybe monad into a certain value or error
certain :: Maybe a -> a
certain (Just a) = a
certain Nothing = error "Tried to make a Nothing monad certain"

--useful string literal handler function

--gets the index of the last quote for string literals
endQuoteIndex :: [Int] -> Maybe Int
endQuoteIndex (a : b : rest) = if (a + 1) /= b then Just a else endQuoteIndex rest
endQuoteIndex (a : []) = Just a
endQuoteIndex _ = Nothing

--gets a prefix of at most size n from an array, doesn't fail if there aren't that many elements
getPrefix :: Int -> [a] -> [a]
getPrefix 0 array = []
getPrefix n [] = []
getPrefix n (a : rest) = a : getPrefix (n - 1) rest

innerStringLiteral :: String -> String
innerStringLiteral [] = []
innerStringLiteral ('\'' : '\'' : rest) = '\'' : innerStringLiteral rest
innerStringLiteral (c : rest) = c : innerStringLiteral rest

getStringLiteral :: String -> (Maybe String, Int)
getStringLiteral s | head s == '\'' = let end = endQuoteIndex (elemIndices '\'' (drop 1 s)) in
                                        if end == Nothing then (Nothing, 0)
                                        else  (Just (innerStringLiteral (getPrefix (certain end) (drop 1 s))), certain end)
getStringLiteral _ = (Nothing, 0)

-- Int parameter is used for line counting
lexer :: String -> Int -> [Token]
lexer s lineCount | s == [] = []
                  | fst (getTokenForString (map toLower s)) /= Nothing =
                                                    let tokString = getTokenForString (map toLower s) in
                                                    certain (fst tokString) : lexer (drop (snd tokString) s) lineCount

                  | head s == '\n' = lexer (drop 1 s) (lineCount + 1)
                  | head s == '{' = let commentSpan = span (/= '}') s in lexer (drop 1 (snd commentSpan)) lineCount
                  --drop whitespace if we're not in a string
                  | isWhiteSpace (head s) = lexer (drop 1 s) lineCount
                  --if we get an alpha we're at an identifier
                  | isAlpha (head s) = let x = (span isAlphaNum s) in TokenIdentifier (map toLower (fst x)) : lexer (snd x) lineCount
                  -- integer constant lexer
                  | isDigit (head s) = let x = span isDigit s in TokenIntLiteral (read (fst x) :: Int) : lexer (snd x) lineCount
                  -- lex string literal
                  | getStringLiteral s /= (Nothing, 0) = let x = getStringLiteral s in TokenStringLiteral ((certain . fst) x) : lexer (drop (snd x + 2) s) lineCount
                  --if we encounter something that isn't empty and we've not matched it it's an error
                  | s /= [] = error ("Lexer error at line " ++ show lineCount ++ " around your code:\"" ++ take 5 s ++ "\"")

lexer [] lineCount = []
lexer _ _ = []
