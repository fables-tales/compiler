module IRTypes where
import ParserTypes

data IRForm = WriteInt {register :: Int} | WriteReal {register :: Int}
              | LoadImmediateInt {register,value :: Int} | LoadImmediateReal {register :: Int, fvalue :: Float}
              | WriteString {location :: Int} | DataPseudo Int | Halt | MemoryStore {reg,memoryAddr :: Int}
              | MemoryLoad {reg,memoryAddr :: Int} | IToR {reg :: Int} | RToI {reg :: Int}
              | IRAdd {r1,r2,r3 :: Int}
              | IRDiv {r1,r2,r3 :: Int}
              | IRMul {r1,r2,r3 :: Int}
              | IRSub {r1,r2,r3 :: Int}
              deriving (Show, Read, Eq)

data IRExpType = TInt | TReal deriving (Show, Read, Eq)

