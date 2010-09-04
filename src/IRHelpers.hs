module IRHelpers where
import Data.Maybe
import Data.List

import IRTypes
import ParserTypes

--produces 4 data IRs for each identifier
_produceData :: Identifier -> [IRForm]
_produceData id = [DataPseudo 0, DataPseudo 0, DataPseudo 0, DataPseudo 0]

--allocates declaration sections in ir form
allocateDeclarations :: [Declaration] -> [IRForm]
allocateDeclarations (Declaration ids t : rest) = concatMap _produceData ids ++ allocateDeclarations rest
allocateDeclarations [] = []

--prints an int as 'R' + int for register usage
regToString :: Int -> String
regToString a = 'R' : show a

--produces instructions to zero a register
zero :: Int -> String
zero a = "\nXOR " ++ regToString a ++ " " ++ regToString a ++ " " ++ regToString a

--compute the size of a serialized string
sizeAdd :: String -> Int -> Int
sizeAdd str size = length str + size + 1

--the round size fo a data table
roundDataSize :: Int -> [IRForm] -> [IRForm]
roundDataSize round values = values ++ (replicate (round - (length values `mod` round)) (DataPseudo 0))

--gets the size of the string section, use to offset declarations
stringSectionSize :: [((String, Int), [IRForm])] -> Int
stringSectionSize a = length (roundDataSize 4 (concatMap snd a))

--convert a type to an IRIRExpType
toIRExpType :: Type -> IRExpType
toIRExpType RealType = TReal
toIRExpType IntType = TInt

--writes a table to irform
writeTable :: [((String, Int), [IRForm])] -> [IRForm]
writeTable a = let tbl = concatMap snd a in roundDataSize 4 tbl

asmMapping :: [(MathOp,String)]
asmMapping = [
                (AddReal, "ADDR"),
                (SubReal, "SUBR"),
                (MulReal, "MULR"),
                (DivReal, "DIVR"),
                (AddInt, "ADD"),
                (SubInt, "SUB"),
                (MulInt, "MUL"),
                (DivInt, "DIV")
             ]

toAsm :: MathOp -> String
toAsm op = (snd . fromJust) (find ((== op) . fst) asmMapping)
