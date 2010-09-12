module IRTypes where
import ParserTypes

data MathOp = AddReal | SubReal | MulReal | DivReal
              | AddInt | SubInt | MulInt | DivInt deriving (Show, Read, Eq)

data IRForm = WriteInt {register :: Int} | WriteReal {register :: Int}
              | LoadImmediateInt {register,value :: Int} | LoadImmediateReal {register :: Int, fvalue :: Float}
              | WriteString {location :: Int} | DataPseudo Int | Halt | MemoryStore {reg,memoryAddr :: Int}
              | MemoryLoad {reg,memoryAddr :: Int} | IToR {reg :: Int} | RToI {reg :: Int}
              | DoMath {op :: MathOp, r1,r2,r3 :: Int} | Label String
              | Zero {reg :: Int}
              --condition to branch on and register to compare
              | Br {cond :: BrCondition, reg :: Int, label :: String}
              deriving (Show, Read, Eq)

data BrCondition = LtZ | LtZR | GeqZ | GeqZR | EqZ | EqZR | NEqZ | NEqZR deriving (Show, Read, Eq)

data IRExpType = TInt | TReal deriving (Show, Read, Eq, Ord)

