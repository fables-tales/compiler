module IR where
import ParserTypes
import Data.Char
import Data.Maybe
import Data.List
import IRHelpers
import IRTypes

--convert a string to data pesudo instructions
serializeString :: String -> [IRForm]
serializeString str = map (DataPseudo . ord) str ++ [DataPseudo 0]

--first pass over tree to generate string literal section
dataMapTable :: [Statement] -> Int -> [((String, Int), [IRForm])]
dataMapTable (WriteS str : rest) size = ((str,size),serializeString str) : dataMapTable rest (sizeAdd str size)
dataMapTable _ _ = []

--gets the location of a string literal in the data table
findStringLocation :: [((String, Int), [IRForm])] -> String -> Int
findStringLocation table a = let triple = find ((== a) . fst . fst) table in
                        if triple == Nothing then -1
                        else let certainTriple = fromJust triple in (snd . fst) certainTriple

--gets the type of a specific variable
getType :: [Declaration] -> Identifier -> IRExpType
getType (Declaration ids t : rest) id = if (find (== id) ids == Nothing) then
                                        getType rest id else (toIRExpType t)
getType [] id = error ("attempted to lookup unknown identifier " ++ show id)

--selects treal if either is treal, else int
selectRichestType :: IRExpType -> IRExpType -> IRExpType
selectRichestType a b = if TReal `elem` [a,b] then TReal else TInt

irForOp :: BinOp -> Int -> IRForm
irForOp Add a = IRAdd a a (a+1)
irForOp Divide a = IRDiv a a (a+1)
irForOp Subtract a = IRSub a a (a+1)
irForOp Multiply a = IRMul a a (a+1)

--convert an expression to ir form, put result in second integer arg
expToIr :: Expression -> [Declaration] -> Int -> Int -> ([IRForm],IRExpType)
expToIr (TermConstant (IntegerLiteral a)) decs offest resultReg = ([LoadImmediateInt resultReg a], TInt)
expToIr (TermConstant (RealLiteral a)) decs offset resultReg = ([LoadImmediateReal resultReg a],TReal)
expToIr (TermVar (id)) decs offset resultReg = ([MemoryLoad resultReg ((variableOffset decs id) + offset)], getType decs id)
expToIr (Op a child1 child2) decs offset resultReg = let
                                                        c1Pair = expToIr child1 decs offset resultReg
                                                        c2Pair = expToIr child2 decs offset (resultReg + 1)
                                                     in
                                                        (fst c1Pair ++ fst c2Pair ++ [irForOp a resultReg], selectRichestType (snd c1Pair) (snd c2Pair))

--get the offset from the base of the declaration section of a specific identifier
variableOffset :: [Declaration] -> Identifier -> Int
variableOffset (Declaration ids t : rest) id = if findIndex (== id) ids == Nothing then
                                            (4 * length ids) + variableOffset rest id
                                            else 4 * (fromJust (findIndex (== id) ids))
variableOffset [] id = error ("attempted to lookup unknown identifier " ++ show id)

--get the absolute location of a specific variable in data memory
varLocation :: Int -> [Declaration] -> Identifier -> Int
varLocation stringSectionSize decs id = stringSectionSize + variableOffset decs id

--convert array of statements to ir form
_toIrForm :: [Statement] -> [((String, Int), [IRForm])] -> [Declaration] -> [IRForm]

_toIrForm (WriteExp a : rest) stringTable decs = let irPair = expToIr a decs (stringSectionSize stringTable) 0 in
                                            fst irPair
                                            ++ (if snd irPair == TReal then WriteReal 0 else WriteInt 0)
                                            :  _toIrForm rest stringTable decs

_toIrForm (WriteLn : rest) stringTable decs = WriteString 0 : _toIrForm rest stringTable decs
_toIrForm (WriteS s : rest) stringTable decs = WriteString (findStringLocation stringTable s) : _toIrForm rest stringTable decs

_toIrForm (Assign id exp : rest) stringTable decs = let
                                                    dataOffset = stringSectionSize stringTable
                                                    pair = expToIr exp decs dataOffset 0
                                                  in
                                           fst pair ++
                                           (if snd pair == TReal && getType decs id == TInt then [RToI 0] else []) ++
                                           (if snd pair == TInt && getType decs id == TReal then [IToR 0] else []) ++
                                           MemoryStore 0 (varLocation dataOffset decs id) :
                                           _toIrForm rest stringTable decs

_toIrForm (a : rest) stringTable decs = _toIrForm rest stringTable decs
_toIrForm [] stringTable decs = Halt : writeTable stringTable ++ allocateDeclarations decs

--convert program to ir form
toIrForm :: Program -> [IRForm]
toIrForm (Program name (Block decs statements)) = _toIrForm statements ((("\n",0),[DataPseudo 10, DataPseudo 0]) : dataMapTable statements 2) decs

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
_toAssembly (IRAdd a b c : rest) = "ADD" ++ regToString a ++ " " ++ regToString b ++ " " ++ regToString c ++ "\n" ++ _toAssembly rest
_toAssembly (IRSub a b c : rest) = "SUB" ++ regToString a ++ " " ++ regToString b ++ " " ++ regToString c ++ "\n" ++ _toAssembly rest
_toAssembly (IRMul a b c : rest) = "MUL" ++ regToString a ++ " " ++ regToString b ++ " " ++ regToString c ++ "\n" ++ _toAssembly rest
_toAssembly (IRDiv a b c : rest) = "DIV" ++ regToString a ++ " " ++ regToString b ++ " " ++ regToString c ++ "\n" ++ _toAssembly rest
_toAssembly [] = []

--returns an integer that is not the passed one
--used for finding a register for use that isn't occupied
findSpareReg :: Int -> Int
findSpareReg a = a + 1

toAssembly :: [IRForm] -> String
toAssembly = _toAssembly
