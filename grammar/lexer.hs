data Token = TokenIdentifier String |
             --program start and end
             TokenProgram String |
             TokenEndProgram |

             --constant tokens
             TokenConstantInt Int |
             TokenConstantReal Float |

             --operators
             TokenPlus | TokenMinus |
             TokenMul | TokenDiv |

             --comment stuff - nb CommentBlocks are possibly multiline
             TokenStartComment | TokenEndComment |
             TokenCommentBlock String |

             -- strings
             TokenSingleQuote |
             TokenStringContents String |

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
stringTokens = [
                        ("+", TokenPlus),
                        ("-", TokenMinus),
                        ("*", TokenMul),
                        ("/", TokenDiv),
                        (":=", TokenAssign),
                        ("=", TokenEq),
                        ("<", TokenLess),
                        ("<=", TokenLessEq),
                        (">", TokenGreater),
                        (">=", TokenGreaterEq),
                        ("(", TokenOb),
                        (")", TokenCb),
                        ("'", TokenSingleQuote),
                        ("read", TokenRead),
                        ("write", TokenWrite),
                        ("writeln", TokenWriteLn),
                        (";", TokenSemiColon),
                        ("var", TokenVar),
                        ("if", TokenIf),
                        ("else", TokenElse)]

