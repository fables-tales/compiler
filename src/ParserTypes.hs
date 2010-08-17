module ParserTypes where

data Program = Program String Block deriving (Show, Read, Eq)

data Identifier = VarIdentifier String deriving (Show, Read, Eq)

data Block = Block [Declaration] [Statement] deriving (Show, Read, Eq)

data Declaration = Declaration [Identifier] Type deriving (Show, Read, Eq)

data Type = RealType | IntType deriving (Show, Read, Eq)

data Statement = Assign Identifier Expression
                 | Read Identifier | ReadInt Identifier | ReadReal Identifier
                 | WriteExp Expression | WriteS String | WriteLn
                 | If Comparison [Statement] | IfElse Comparison [Statement] [Statement]
                 | RepeatUntil Comparison [Statement] deriving (Show, Read, Eq)

data Comparison = Comparison Relation Expression Expression deriving (Show, Read, Eq)

data Relation = RelGreater | RelGreaterEq | RelEq | RelNeq | RelLessEq | RelLess deriving (Show, Read, Eq)

data UnaryOp = UnaryPlus | UnaryMinus deriving (Show, Read, Eq)

data BinOp = Add | Divide | Subtract | Multiply deriving (Show, Read, Eq)

data Expression = Op BinOp Expression Expression |
                  TermVar Identifier | TermConstant NumberLiteral deriving (Show, Read, Eq)

data NumberLiteral = RealLiteral Float | IntegerLiteral Int deriving (Show, Read, Eq)

_negate :: Expression -> Expression
_negate a = Op Multiply (TermConstant (IntegerLiteral (-1))) a

parseError a = error (show (head a))
