module Lexer where
import Data.List
import Data.Char

isWhiteSpace :: Char -> Bool
isWhiteSpace x = x == ' ' || x == '\t' || x == '\n'

data Token = TokenIdentifier String |
             --program start
             TokenProgram |

             --int literal, reals aren't lexed as tokens, but are parsed instead
             TokenIntLiteral Int |

             -- '.' and 'e' for floating point
             TokenDot | TokenE |

             --operators
             TokenPlus | TokenMinus |
             TokenMul | TokenDiv |

             --comment stuff - nb CommentBlocks are possibly multiline
             TokenStartComment | TokenEndComment |
             TokenCommentBlock String |

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
                        (".", TokenDot),
                        ("e", TokenE)
               ]

getTokenForString :: String -> ((Maybe Token), Int)
getTokenForString s = let pair = (find ((`isPrefixOf` s) . fst) stringTokens) in
                        if pair == Nothing then (Nothing, 0)
                        else let certainPair = certain pair in
                            (Just (snd certainPair), length (fst certainPair))

lexer :: String -> [Token]
lexer s | (fst (getTokenForString s) /= Nothing) = let tokString = getTokenForString s in
                                                    certain (fst (tokString)) : lexer (drop (snd tokString) s)
lexer [] = []
--turn a maybe monad into a certain value or error
certain :: Maybe a -> a
certain (Just a) = a
certain Nothing = error "Tried to make a Nothing monad certain"

