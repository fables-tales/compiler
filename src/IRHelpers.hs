module IRHelpers where
import Data.Maybe
import Data.List
import Data.Char

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
regToString reg = 'R' : show reg

--produces instructions to zero a register
zero :: Int -> String
zero reg = "\n;zeroing register" ++ show reg ++ "\nXOR " ++ regToString reg ++ " " ++ regToString reg ++ " " ++ regToString reg

--compute the size of a serialized string
sizeAdd :: String -> Int -> Int
sizeAdd str size = length str + size + 1

--the round size fo a data table
roundDataSize :: Int -> [IRForm] -> [IRForm]
roundDataSize round values = values ++ replicate (round - (length values `mod` round)) (DataPseudo 0)

--gets the size of the string section, use to offset declarations
stringSectionSize :: [((String, Int), [IRForm])] -> Int
stringSectionSize = length . roundDataSize 4 . concatMap snd

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


--convert a string to data pesudo instructions
serializeString :: String -> [IRForm]
serializeString str = map (DataPseudo . ord) str ++ [DataPseudo 0]

getNewSizeForTable :: [((String, Int), [IRForm])] -> Int -> Int
getNewSizeForTable [] a = a
getNewSizeForTable table a = sizeAdd ((fst . fst . last) table) ((snd . fst . last) table)

--first pass over tree to generate string literal section
dataMapTable :: [Statement] -> Int -> [((String, Int), [IRForm])]
dataMapTable (WriteS str : rest) size = ((str,size),serializeString str) : dataMapTable rest (sizeAdd str size)
dataMapTable (If cmp statements : rest) size = let table = dataMapTable statements size in
                                                table ++ dataMapTable rest (getNewSizeForTable table size)

dataMapTable (IfElse cmp s1 s2 : rest) size = let
                                                table1 = dataMapTable s1 size
                                                table2 = dataMapTable s2 (getNewSizeForTable table1 size)
                                              in table1 ++ table2 ++ dataMapTable rest (getNewSizeForTable table2 size)

dataMapTable (RepeatUntil cmp s1 : rest) size = let table = dataMapTable s1 size in
                                                table ++ dataMapTable rest (getNewSizeForTable table size)
dataMapTable (a : rest) size = dataMapTable rest size
dataMapTable [] size = []

--gets the location of a string literal in the data table
findStringLocation :: [((String, Int), [IRForm])] -> String -> Int
findStringLocation stringTable string = let triple = find ((== string) . fst . fst) stringTable in
                        if triple == Nothing then error "couldn't find string"
                        else let certainTriple = fromJust triple in (snd . fst) certainTriple

--gets the type of a specific variable
getType :: [Declaration] -> Identifier -> IRExpType
getType (Declaration ids t : rest) id = if find (== id) ids == Nothing then
                                        getType rest id else toIRExpType t
getType [] id = error ("attempted to lookup unknown identifier " ++ show id)

--selects treal if either is treal, else int
selectRichestType :: IRExpType -> IRExpType -> IRExpType
selectRichestType a b = if TReal `elem` [a,b] then TReal else TInt

checkedCast :: IRExpType -> IRExpType -> Int -> [IRForm]
checkedCast richest t reg = [IToR reg | richest > t]

irOperations :: [((BinOp,IRExpType),MathOp)]
irOperations = [
            ((Add,TReal),AddReal),
            ((Divide,TReal),DivReal),
            ((Multiply,TReal),MulReal),
            ((Subtract,TReal),SubReal),
            ((Add,TInt),AddInt),
            ((Divide,TInt),DivInt),
            ((Multiply,TInt),MulInt),
            ((Subtract,TInt),SubInt)
          ]

typedOpFor :: IRExpType -> BinOp -> Int -> Int -> Int -> IRForm
typedOpFor t op a b c = let opType = (op,t) in DoMath ((snd . fromJust) (find (( == opType) . fst) irOperations)) a b c

irForOp :: BinOp -> Int -> IRExpType -> IRExpType -> IRExpType -> [IRForm]
irForOp op a richest c1 c2 = checkedCast richest c1 a ++ checkedCast richest c2 (a+1) ++ [typedOpFor richest op a a (a+1)]

--get the offset from the base of the declaration section of a specific identifier
variableOffset :: [Declaration] -> Identifier -> Int
variableOffset (Declaration ids t : rest) id = if findIndex (== id) ids == Nothing then
                                            (4 * length ids) + variableOffset rest id
                                            else 4 * fromJust (findIndex (== id) ids)
variableOffset [] id = error ("attempted to lookup unknown identifier " ++ show id)

--get the absolute location of a specific variable in data memory
varLocation :: Int -> [Declaration] -> Identifier -> Int
varLocation stringSectionSize decs id = stringSectionSize + variableOffset decs id


--returns an integer that is not the passed one
--used for finding a register for use that isn't occupied
findSpareReg :: Int -> Int
findSpareReg a = a + 1


--opposite is a bit of a misnomer, it's whatever matches everything
--the argument doesn't
opposite :: Relation -> Relation
opposite RelGreater = RelLessEq
opposite RelGreaterEq = RelLess
opposite RelEq = RelNeq
opposite RelNeq = RelEq
opposite RelLessEq = RelGreater
opposite RelLess = RelGreaterEq
