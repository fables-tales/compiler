module IR where
import Data.Char
import Data.Maybe
import Data.List

import IRHelpers
import IRTypes
import ParserTypes

--convert an expression to ir form, put result in second integer arg
--the offset parameter is the offset of the variable block of memory after the stirng block
expToIr :: Expression -> [Declaration] -> Int -> Int -> ([IRForm],IRExpType)
expToIr (TermConstant (IntegerLiteral a)) decs offest resultReg = ([LoadImmediateInt resultReg a], TInt)
expToIr (TermConstant (RealLiteral a)) decs offset resultReg = ([LoadImmediateReal resultReg a],TReal)
expToIr (TermVar id) decs offset resultReg = ([MemoryLoad resultReg (variableOffset decs id + offset)], getType decs id)
expToIr (Op a child1 child2) decs offset resultReg = let
                                                        c1Pair = expToIr child1 decs offset resultReg
                                                        c2Pair = expToIr child2 decs offset (resultReg + 1)
                                                        richest = selectRichestType (snd c1Pair) (snd c2Pair)
                                                     in
                                                        (fst c1Pair ++
                                                         fst c2Pair ++
                                                         irForOp a resultReg richest (snd c1Pair) (snd c2Pair), richest)

isLabel :: IRForm -> Bool
isLabel (Label a) = True
isLabel _ = False

numLabels :: [IRForm] -> Int
numLabels a = sum (map (\x -> if isLabel x then 1 else 0) a)

cmpExps :: Expression -> Expression -> [Declaration] -> Int -> [IRForm]
cmpExps e1 e2 decs offset = let
                             se1 = expToIr e1 decs offset 0
                             se2 = expToIr e2 decs offset 1
                            in
                             fst se1 ++
                             fst se2 ++
                             checkedCast (snd se1) (snd se2) 1 ++
                             checkedCast (snd se2) (snd se1) 0 ++
                             irForOp Subtract 0 (selectRichestType (snd se1) (snd se2)) (snd se1) (snd se2)


--mapping of expressions into branches, note that to save instructions
--expressions are reversed if we don't have a native way of executing the
--specific comparison
cmpTable = [
            ((RelLess,TReal),      (True,LtZR)),
            ((RelLess,TInt),       (True,LtZ)),
            ((RelGreaterEq,TReal), (True,GeqZR)),
            ((RelGreaterEq,TInt),  (True,GeqZ)),
            ((RelLessEq,TReal),    (False,GeqZR)),
            ((RelLessEq,TInt),     (False,GeqZ)),
            ((RelGreater,TReal),   (False,LtZR)),
            ((RelGreater,TInt),    (False,LtZ)),
            ((RelEq,TReal),        (True, EqZR)),
            ((RelEq,TInt),         (True, EqZ)),
            ((RelNeq,TReal),       (True, NEqZR)),
            ((RelNeq,TInt),        (True, NEqZ))
           ]

_getComparisonFor :: Relation -> IRExpType -> (Bool, BrCondition)
_getComparisonFor rel t = let lookup = (rel,t) in (snd . fromJust) (find ((== lookup) . fst) cmpTable)

getComparisonFor :: Relation -> Expression -> Expression -> [Declaration] -> Int -> (Bool, BrCondition)
getComparisonFor rel e1 e2 decs offset = let
                                            se1 = expToIr e1 decs offset 0
                                            se2 = expToIr e2 decs offset 0
                                         in
                                            _getComparisonFor rel (selectRichestType (snd se1) (snd se1))

serializeComparison :: Comparison -> String -> Bool -> [Declaration] -> Int -> [IRForm]
serializeComparison (Comparison rel e1 e2) labelBranch branchOn decs offset = let
                                                                        rel2 = if branchOn then rel else opposite rel
                                                                        actualComparison = getComparisonFor rel2 e1 e2 decs offset
                                                                        flip = (not . fst) actualComparison
                                                                      in
                                                                      (if not flip then cmpExps e1 e2 decs offset
                                                                      else cmpExps e2 e1 decs offset) ++
                                                                      [Br (snd actualComparison) 0 labelBranch]

--convert array of statements to ir form
_toIrForm :: [Statement] -> [((String, Int), [IRForm])] -> [Declaration] -> Int -> [IRForm]

_toIrForm (WriteExp a : rest) stringTable decs labelCount = let irPair = expToIr a decs (stringSectionSize stringTable) 0 in
                                            fst irPair
                                            ++ (if snd irPair == TReal then WriteReal 0 else WriteInt 0)
                                            :  _toIrForm rest stringTable decs labelCount

_toIrForm (WriteLn : rest) stringTable decs labelCount = WriteString 0 : _toIrForm rest stringTable decs labelCount
_toIrForm (WriteS s : rest) stringTable decs labelCount = WriteString (findStringLocation stringTable s) : _toIrForm rest stringTable decs labelCount

_toIrForm (Assign id exp : rest) stringTable decs labelCount = let
                                                    dataOffset = stringSectionSize stringTable
                                                    pair = expToIr exp decs dataOffset 0
                                                  in
                                               fst pair ++
                                               [RToI 0 | snd pair == TReal && getType decs id == TInt] ++
                                               [IToR 0 | snd pair == TInt && getType decs id == TReal] ++
                                               MemoryStore 0 (varLocation dataOffset decs id) :
                                               _toIrForm rest stringTable decs labelCount

_toIrForm (If comparison statements : rest) stringTable decs labelCount =
                                                         let
                                                            labelName = "if" ++ show labelCount
                                                            ir = _toIrForm statements stringTable decs (labelCount + 1)
                                                         in
                                                         --on false, branch to after the if statement
                                                         serializeComparison comparison (labelName ++ "aft") False decs (stringSectionSize stringTable) ++
                                                         Label (labelName ++ "in") : ir ++
                                                         Label (labelName ++ "aft") : _toIrForm rest stringTable decs (labelCount + 1 + numLabels ir)
_toIrForm (IfElse comparison s1 s2 : rest) stringTable decs labelCount =
                                                        let
                                                            labelName = "if" ++ show labelCount
                                                            ir1 = _toIrForm s1 stringTable decs (labelCount + 1)
                                                            ir2 = _toIrForm s2 stringTable decs (labelCount + 1 +  numLabels ir1)
                                                        in
                                                        serializeComparison comparison (labelName ++ "else") False decs (stringSectionSize stringTable) ++
                                                        Label (labelName ++ "in") : ir1 ++
                                                        [Zero 0,Br EqZ 0 (labelName ++ "aft")] ++
                                                        Label (labelName ++ "else") : ir2
                                                        ++ Label (labelName ++ "aft") :
                                                        _toIrForm rest stringTable decs (labelCount + 1 + numLabels ir2)

_toIrForm (RepeatUntil comparison smts : rest) stringTable decs labelCount =
                                                        let
                                                            labelName = "rep" ++ show labelCount
                                                            ir1 = _toIrForm smts stringTable decs (labelCount + 1)
                                                        in
                                                        Label (labelName ++ "bef") : ir1 ++
                                                        serializeComparison comparison (labelName ++ "bef") False decs (stringSectionSize stringTable) ++
                                                        _toIrForm rest stringTable decs (labelCount + 1 + numLabels ir1)

_toIrForm (Read id : rest) stringTable decs labelCount = let
                                                            idType = getType decs id
                                                            idLocation = varLocation (stringSectionSize stringTable) decs id
                                                         in
                                                         (if idType == TInt then ReadInt else ReadFloat) 0 idLocation : _toIrForm rest stringTable decs labelCount

_toIrForm (a : rest) stringTable decs labelCount = _toIrForm rest stringTable decs labelCount
_toIrForm [] stringTable decs labelCount = []


brTable :: [(BrCondition,String)]
brTable =  [
             (LtZR,  "BLTZR"),
             (LtZ,   "BLTZ"),
             (GeqZ,  "BGEZ"),
             (GeqZR, "BGEZR"),
             (EqZ,   "BEQZ"),
             (EqZR,  "BEQZR"),
             (NEqZ,  "BNEZ"),
             (NEqZR, "BNEZR")
           ]

brAsm :: BrCondition -> String
brAsm a = (snd . fromJust) (find ((== a) . fst) brTable)

--convert program to ir form
toIrForm :: Program -> [IRForm]
toIrForm (Program name (Block decs statements)) = let table = (("\n",0),[DataPseudo 10, DataPseudo 0]) : dataMapTable statements 2 in
                                                  _toIrForm statements table decs 0 ++ (Halt : writeTable table ++ allocateDeclarations decs)

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
_toAssembly (DoMath op a b c : rest) = toAsm op ++ " " ++ regToString a ++ " " ++ regToString b ++ " " ++ regToString c ++ "\n" ++ _toAssembly rest
_toAssembly (Br cond reg label : rest) = brAsm cond ++ " " ++  regToString reg ++ " " ++ label ++ "\n" ++  _toAssembly rest
_toAssembly (Zero reg : rest) = zero reg ++ "\n" ++ _toAssembly rest
_toAssembly (Label a : rest) = a ++ ":\n" ++ _toAssembly rest
_toAssembly (ReadInt reg location : rest) = "RD " ++ regToString reg ++ "\n" ++ _toAssembly [MemoryStore reg location] ++ _toAssembly rest
_toAssembly (ReadFloat reg location : rest) = "RDR " ++ regToString reg ++ "\n" ++ _toAssembly [MemoryStore reg location] ++ _toAssembly rest
_toAssembly (_ : rest) = _toAssembly rest
_toAssembly [] = []


toAssembly :: [IRForm] -> String
toAssembly = _toAssembly
