module Lexer where
import Data.List
import Data.Char

isWhiteSpace :: Char -> Bool
isWhiteSpace x = x `elem` "\n \t\r"

matches :: Eq a =>a -> [a] -> Int
matches a [] = 0
matches a (item : rest) = if item == a then 1 + matches a rest else matches a rest

data Token = TokenIdentifier String |
             --separates things
             TokenComma |
             --program start
             TokenProgram |

             --int literal, reals are lexed in stage 2 of the lexer
             TokenIntLiteral Int |
             TokenRealLiteral Float |

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
             TokenLess deriving (Show, Eq, Ord)
--deals with all tokens that are simple strings - nb cast everything to lowercase because we can.

--do the reverse sort in order to get longest tokens first, this is important
--because we do earliest match in gettoken and some tokens could be prefixes of longer tokens
stringTokens :: [(String, Token)]
stringTokens = reverse ( sort [
                        ("+", TokenPlus), ("-", TokenMinus),
                        ("*", TokenMul), ("/", TokenDiv),
                        (":=", TokenAssign),
                        ("=", TokenEq), ("!=", TokenNeq),
                        ("<", TokenLess),("<=", TokenLessEq),
                        (">", TokenGreater), (">=", TokenGreaterEq),
                        ("(", TokenOb), (")", TokenCb),
                        ("read", TokenRead),
                        ("writeln", TokenWriteLn),
                        ("write", TokenWrite),
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
               ])

--extracts a token from a string from the above simple strings map
--also gets the legnth of the token that's been lexed
getTokenForString :: String -> (Maybe Token, Int)
getTokenForString s = let pair = (find ((`isPrefixOf` s) . fst) stringTokens) in
                        if pair == Nothing then (Nothing, 0)
                        else let certainPair = certain pair in
                            (Just (snd certainPair), length (fst certainPair))

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
_lexer :: String -> Int -> [Token]
_lexer s lineCount | s == [] = []
                  -- match simple string tokens
                  | fst (getTokenForString (map toLower s)) /= Nothing =
                                                    let tokString = getTokenForString (map toLower s) in
                                                    certain (fst tokString) : _lexer (drop (snd tokString) s) lineCount

                  --skip newlines but increment linecount
                  | head s == '\n' = _lexer (drop 1 s) (lineCount + 1)

                  | head s == '{' = let commentSpan = span (/= '}') s in
                                        _lexer (drop 1 (snd commentSpan)) (lineCount + matches '\n' (fst commentSpan))

                  --drop whitespace if we're not in a string
                  | isWhiteSpace (head s) = _lexer (drop 1 s) lineCount

                  --if we get an alpha we're at an identifier
                  | isAlpha (head s) = let x = (span isAlphaNum s) in
                                        TokenIdentifier (map toLower (fst x)) : _lexer (snd x) lineCount

                  -- integer constant
                  | isDigit (head s) = let x = span isDigit s in
                                        if (read (fst x) :: Int) > 2^32 then error ("int literal encountered bigger than 2^32 at line " ++ show lineCount)
                                        else TokenIntLiteral (read (fst x) :: Int) : _lexer (snd x) lineCount

                  -- lex string literal
                  | getStringLiteral s /= (Nothing, 0) = let x = getStringLiteral s in
                                                TokenStringLiteral ((certain . fst) x) :
                                                --drop x + 2 chars because the literal doesn't include the
                                                --beginning or end quotes
                                                                            --increment linecount by the number of matches in the string
                                                _lexer (drop (snd x + 2) s) (lineCount + matches '\n' ((certain . fst) x))

                  --if we encounter something that isn't empty and we've not matched it it's an error
                  | s /= [] = error ("Lexer error at line " ++ show lineCount ++ " around your code:\"" ++ take 5 s ++ "\"")

--if we've run out of chars we're done
_lexer [] lineCount = []

--wildcard match, error because something unexpected happened
_lexer _ count = error ("hit a wildcard around line " ++ show count)

fl :: (Integral a) => a -> Float
fl a = fromIntegral a :: Float

logBaseTen :: Int -> Float
logBaseTen 0 = 0
logBaseTen a = logBase 10.0 (fl a) :: Float

computeFloat :: Int -> Int -> Float
computeFloat a b = fl a + fl b / fl (10 ^ (floor (logBaseTen b) + 1))

exp10 :: Int -> Float
exp10 a | a >= 0 = fl (10 ^ a)
		| a < 0 = 1.0/fl (10^(-a))

--lex real numbers to real tokens
lexreals :: [Token] -> [Token]
lexreals (TokenIntLiteral a : TokenDot : TokenIntLiteral b : TokenE : TokenMinus : TokenIntLiteral c : rest) = TokenRealLiteral (computeFloat a b * exp10 (-c)) : lexreals rest
lexreals (TokenIntLiteral a : TokenDot : TokenIntLiteral b : TokenE : TokenIntLiteral c : rest) = TokenRealLiteral (computeFloat a b * exp10 c) : lexreals rest
lexreals (TokenIntLiteral a : TokenDot : TokenIntLiteral b : rest) = TokenRealLiteral (computeFloat a b) : lexreals rest
lexreals [] = []
lexreals (a : rest) = a : lexreals rest

lexer :: String -> [Token]
lexer a = lexreals (_lexer a 1)
