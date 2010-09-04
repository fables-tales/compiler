module IRTypes where
import ParserTypes

data IRForm = WriteInt {register :: Int} | WriteReal {register :: Int}
              | LoadImmediateInt {register,value :: Int} | LoadImmediateReal {register :: Int, fvalue :: Float}
              | WriteString {location :: Int} | DataPseudo Int | Halt | MemoryStore {reg,memoryAddr :: Int}
              | MemoryLoad {reg,memoryAddr :: Int} | IToR {reg :: Int} | RToI {reg :: Int}
              | AddReal {r1,r2,r3 :: Int}
              | SubReal {r1,r2,r3 :: Int}
              | MulReal {r1,r2,r3 :: Int}
              | DivReal {r1,r2,r3 :: Int}
              | AddInt {r1,r2,r3 :: Int}
              | SubInt {r1,r2,r3 :: Int}
              | MulInt {r1,r2,r3 :: Int}
              | DivInt {r1,r2,r3 :: Int}
              deriving (Show, Read, Eq)

data IRExpType = TInt | TReal deriving (Show, Read, Eq, Ord)

