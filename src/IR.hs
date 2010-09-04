module IR where
import ParserTypes
import Data.Char
import Data.Maybe
import Data.List

data IRForm = WriteInt {register :: Int} | WriteReal {register :: Int}
              | LoadImmediateInt {register,value :: Int} | LoadImmediateReal {register :: Int, fvalue :: Float}
              | WriteString {location :: Int} | DataPseudo Int | Halt | MemoryStore {reg,memoryAddr :: Int}
              | MemoryLoad {reg,memoryAddr :: Int} | IToR {reg :: Int} | RToI {reg :: Int} deriving (Show, Read, Eq)


data ExpType = TInt | TReal deriving (Show, Read, Eq)

--compute the size of a serialized string
sizeAdd :: String -> Int -> Int
sizeAdd str size = length str + size + 1

--convert a string to data pesudo instructions
serializeString :: String -> [IRForm]
serializeString str = map (DataPseudo . ord) str ++ [DataPseudo 0]

--first pass over tree to generate string literal data section
dataMapTable :: [Statement] -> Int -> [((String, Int), [IRForm])]
dataMapTable (WriteS str : rest) size = ((str,size),serializeString str) : dataMapTable rest (sizeAdd str size)
dataMapTable _ _ = []

--gets the location of a string literal in the data table
findLocation :: [((String, Int), [IRForm])] -> String -> Int
findLocation table a = let triple = find ((== a) . fst . fst) table in
                        if triple == Nothing then -1
                        else let certainTriple = fromJust triple in (snd . fst) certainTriple

--the round size fo a data table
roundDataSize :: Int -> [IRForm] -> [IRForm]
roundDataSize round values = values ++ (replicate (round - (length values `mod` round)) (DataPseudo 0))

--offset for variable section in data memory
varOffset :: [((String, Int), [IRForm])] -> Int
varOffset a = length (roundDataSize 4 (concatMap snd a))

toExpType :: Type -> ExpType
toExpType RealType = TReal
toExpType IntType = TInt

--writes a table to irform
writeTable :: [((String, Int), [IRForm])] -> [IRForm]
writeTable a = let tbl = concatMap snd a in tbl ++ roundDataSize 4 tbl

--gets the type of a specific variable
getType :: [Declaration] -> Identifier -> ExpType
getType (Declaration ids t : rest) id = if (find (== id) ids == Nothing) then
                                        getType rest id else (toExpType t)
getType [] id = error ("attempted to lookup unknown identifier " ++ show id)

--convert an expression to ir form, always put the result in register 0
expToIr :: Expression -> [Declaration] -> Int -> ([IRForm],ExpType)
expToIr (TermConstant (IntegerLiteral a)) decs offest = ([LoadImmediateInt 0 a], TInt)
expToIr (TermConstant (RealLiteral a)) decs offset = ([LoadImmediateReal 0 a],TReal)
expToIr (TermVar (id)) decs offset = ([MemoryLoad 0 ((decOffset decs id) + offset)], getType decs id)

--offset of a specific identifier in the declaration section
decOffset :: [Declaration] -> Identifier -> Int
decOffset (Declaration ids t : rest) id = if findIndex (== id) ids == Nothing then
                                            (4 * length ids) + decOffset rest id
                                            else 4 * (fromJust (findIndex (== id) ids))
decOffset [] id = error ("attempted to lookup unknown identifier " ++ show id)

--get the location of a variable
varLocation :: Int -> [Declaration] -> Identifier -> Int
varLocation tableSize decs id = tableSize + decOffset decs id

--convert array of statements to ir form
_toIrForm :: [Statement] -> [((String, Int), [IRForm])] -> [Declaration] -> [IRForm]

_toIrForm (WriteExp a : rest) dataTable decs = let irPair = expToIr a decs (varOffset dataTable) in
                                            fst irPair
                                            ++ (if snd irPair == TReal then WriteReal 0 else WriteInt 0)
                                            :  _toIrForm rest dataTable decs

_toIrForm (WriteLn : rest) dataTable decs = WriteString 0 : _toIrForm rest dataTable decs
_toIrForm (WriteS s : rest) dataTable decs = WriteString (findLocation dataTable s) : _toIrForm rest dataTable decs

_toIrForm (Assign id exp : rest) dataTable decs = let
                                                    vOff = varOffset dataTable
                                                    pair = expToIr exp decs vOff
                                                  in
                                           fst pair ++
                                           (if snd pair == TReal && getType decs id == TInt then [RToI 0] else []) ++
                                           MemoryStore 0 (varLocation vOff decs id) :
                                           _toIrForm rest dataTable decs

_toIrForm (a : rest) dataTable decs = _toIrForm rest dataTable decs
_toIrForm [] dataTable decs = Halt : writeTable dataTable ++ allocateDeclarations decs

_produceData :: Identifier -> [IRForm]
_produceData id = [DataPseudo 0, DataPseudo 0, DataPseudo 0, DataPseudo 0]

allocateDeclarations :: [Declaration] -> [IRForm]
allocateDeclarations (Declaration ids t : rest) = concatMap _produceData ids ++ allocateDeclarations rest
allocateDeclarations [] = []

--convert program to ir form
toIrForm :: Program -> [IRForm]
toIrForm (Program name (Block decs statements)) = _toIrForm statements ((("\n",0),[DataPseudo 10, DataPseudo 0]) : dataMapTable statements 2) decs

regToString :: Int -> String
regToString a = 'R' : show a

zero :: Int -> String
zero a = "\nXOR " ++ regToString a ++ " " ++ regToString a ++ " " ++ regToString a

_toAssembly :: [IRForm] -> String
_toAssembly ((WriteInt {register = a}) : rest) = ";writing register r" ++ show a ++ "\nWR " ++ regToString a ++ "\n" ++ _toAssembly rest
_toAssembly ((WriteReal {register = a}) : rest) = ";writing register r" ++ show a ++ "\nWRR " ++ regToString a ++ "\n" ++ _toAssembly rest
_toAssembly ((LoadImmediateInt {register = reg, value = v}) : rest) = ";loading immediate " ++ show v ++ " into register r" ++ show reg
															++ zero reg ++ "\n"
															++ "ADDI " ++ regToString reg ++ " " ++ regToString reg ++ " " ++ show v ++ "\n" ++ _toAssembly rest
_toAssembly ((LoadImmediateReal {register = reg, fvalue = v}) : rest) = ";loading immediate " ++ show v ++ "into register r" ++ show reg
                                                                       ++ "\nMOVIR " ++ regToString reg ++ " " ++ show v ++ "\n" ++ _toAssembly rest
_toAssembly (WriteString {location = loc} : rest) = "WRS " ++ show loc ++ "\n" ++ _toAssembly rest
_toAssembly (Halt : rest) = "HALT\n" ++ _toAssembly rest
_toAssembly (DataPseudo a : rest) = "DATA " ++ show a ++ "\n" ++ _toAssembly rest
_toAssembly (MemoryStore reg addr : rest) = let spare = findSpareReg reg in zero spare ++ "\nSTORE " ++ regToString reg ++ " " ++ regToString spare ++ " " ++ show addr ++ "\n" ++ _toAssembly rest
_toAssembly (MemoryLoad reg addr : rest) = let spare = findSpareReg reg in zero spare ++ "\nLOAD " ++ regToString reg ++ " " ++ regToString spare ++ " " ++ show addr ++ "\n" ++ _toAssembly rest
_toAssembly (IToR reg : rest) = "ITOR " ++ regToString reg ++ " " ++ regToString reg ++ "\n" ++ _toAssembly rest
_toAssembly (RToI reg : rest) = "RTOI " ++ regToString reg ++ " " ++ regToString reg ++ "\n" ++ _toAssembly rest
_toAssembly [] = []

--returns an integer that is not the passed one
--used for finding a register for use that isn't occupied
findSpareReg :: Int -> Int
findSpareReg a = a + 1

toAssembly :: [IRForm] -> String
toAssembly = _toAssembly
