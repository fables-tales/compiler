module IRTypes where
import ParserTypes

data MathOp = AddReal | SubReal | MulReal | DivReal
              | AddInt | SubInt | MulInt | DivInt deriving (Show, Read, Eq)

data IRForm = WriteInt {register :: Int} | WriteReal {register :: Int}
              | LoadImmediateInt {register,value :: Int} | LoadImmediateReal {register :: Int, fvalue :: Float}
              | WriteString {location :: Int} | DataPseudo Int | Halt | MemoryStore {reg,memoryAddr :: Int}
              | MemoryLoad {reg,memoryAddr :: Int} | IToR {reg :: Int} | RToI {reg :: Int}
              | DoMath {op :: MathOp, r1,r2,r3 :: Int} | Label String
              | DoMathImmediate {op :: MathOp, r1,r2,immediate :: Int}
              | Zero {reg :: Int}
              --condition to branch on and register to compare
              | Br {cond :: BrCondition, reg :: Int, label :: String}
              | ReadFloat {reg,location :: Int} | ReadInt {reg,location :: Int}
              deriving (Show, Read, Eq)

data BrCondition = LtZ | LtZR | GeqZ | GeqZR | EqZ | EqZR | NEqZ | NEqZR deriving (Show, Read, Eq)


data Assembly = ADD Int Int Int | DIV Int Int Int | MUL Int Int Int | SUB Int Int Int
                | ADDR Int Int Int | DIVR Int Int Int | MULR Int Int Int | SUBR Int Int Int
                | ADDI Int Int Int | DIVI Int Int Int | MULI Int Int Int | SUBI Int Int Int
                | ITOR Int Int | RTOI Int Int | DATA Int | HALT | WRS Int | RD Int | RDR Int
                | STORE Int Int Int | XOR Int Int Int | LOAD Int Int Int | LABEL String
                | WR Int | WRR Int | MOVIR Int Float | BLTZR Int String | BLTZ Int String
                | BGEZ Int String | BGEZR Int String | BEQZ Int String | BEQZR Int String
                | BNEZ Int String | BNEZR Int String | Lbl String deriving (Show, Read, Eq)



data IRExpType = TInt | TReal deriving (Show, Read, Eq, Ord)

