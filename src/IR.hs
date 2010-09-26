module IR where
import Data.Char
import Data.Maybe
import Data.List

import IRHelpers
import IRTypes
import ParserTypes
import Optimiser

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
                             se1 = expToIr e1 decs offset 1
                             se2 = expToIr e2 decs offset 2
                            in
                             fst se1 ++
                             fst se2 ++
                             checkedCast (snd se1) (snd se2) 2 ++
                             checkedCast (snd se2) (snd se1) 1 ++
                             irForOp Subtract 1 (selectRichestType (snd se1) (snd se2)) (snd se1) (snd se2)


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
                                                                      [Br (snd actualComparison) 1 labelBranch]

--convert array of statements to ir form
_toIrForm :: [Statement] -> [((String, Int), [IRForm])] -> [Declaration] -> Int -> [IRForm]

_toIrForm (WriteExp a : rest) stringTable decs labelCount = let irPair = expToIr a decs (stringSectionSize stringTable) 1 in
                                            fst irPair
                                            ++ (if snd irPair == TReal then WriteReal 1 else WriteInt 1)
                                            :  _toIrForm rest stringTable decs labelCount

_toIrForm (WriteLn : rest) stringTable decs labelCount = WriteString 0 : _toIrForm rest stringTable decs labelCount
_toIrForm (WriteS s : rest) stringTable decs labelCount = WriteString (findStringLocation stringTable s) : _toIrForm rest stringTable decs labelCount

_toIrForm (Assign id exp : rest) stringTable decs labelCount = let
                                                    dataOffset = stringSectionSize stringTable
                                                    pair = expToIr exp decs dataOffset 1
                                                  in
                                               fst pair ++
                                               [RToI 1 | snd pair == TReal && getType decs id == TInt] ++
                                               [IToR 1 | snd pair == TInt && getType decs id == TReal] ++
                                               MemoryStore 1 (varLocation dataOffset decs id) :
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
                                                        [Zero 1,Br EqZ 1 (labelName ++ "aft")] ++
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
                                                         (if idType == TInt then ReadInt else ReadFloat) 1 idLocation : _toIrForm rest stringTable decs labelCount

_toIrForm (a : rest) stringTable decs labelCount = _toIrForm rest stringTable decs labelCount
_toIrForm [] stringTable decs labelCount = []


brTable :: [(BrCondition,Int->String->Assembly)]
brTable =  [
             (LtZR,  BLTZR),
             (LtZ,   BLTZ),
             (GeqZ,  BGEZ),
             (GeqZR, BGEZR),
             (EqZ,   BEQZ),
             (EqZR,  BEQZR),
             (NEqZ,  BNEZ),
             (NEqZR, BNEZR)
           ]

brAsm :: BrCondition -> (Int -> String -> Assembly)
brAsm a = (snd . fromJust) (find ((== a) . fst) brTable)

--convert program to ir form
toIrForm :: Program -> [IRForm]
toIrForm (Program name (Block decs [])) = [Halt]
toIrForm (Program name (Block decs statements)) = let table = (("\n",0),[DataPseudo 10, DataPseudo 0]) : dataMapTable statements 2 in
                                                  Zero 0 : _toIrForm statements table decs 0 ++ (Halt : writeTable table ++ allocateDeclarations decs)

--convert ir to assembly data structure
_toAssembly :: [IRForm] -> [Assembly]
_toAssembly ((WriteInt {register = a}) : rest) = WR a : _toAssembly rest
_toAssembly ((WriteReal {register = a}) : rest) = WRR a : _toAssembly rest
_toAssembly ((LoadImmediateInt {register = reg, value = v}) : rest) = ADDI reg 0 v : _toAssembly rest
_toAssembly ((LoadImmediateReal {register = reg, fvalue = v}) : rest) = MOVIR reg v : _toAssembly rest
_toAssembly (WriteString {location = loc} : rest) = WRS loc : _toAssembly rest
_toAssembly (Halt : rest) = HALT : _toAssembly rest
_toAssembly (DataPseudo a : rest) = DATA a : _toAssembly rest
_toAssembly (MemoryStore reg addr : rest) = let spare = 0 in STORE reg spare addr : _toAssembly rest
_toAssembly (MemoryLoad reg addr : rest) = let spare = 0 in LOAD reg spare addr : _toAssembly rest
_toAssembly (IToR reg : rest) = ITOR reg reg : _toAssembly rest
_toAssembly (RToI reg : rest) = RTOI reg reg : _toAssembly rest
_toAssembly (DoMath op a b c : rest) = (toAsm op False) a b c : _toAssembly rest
_toAssembly (DoMathImmediate op rtarget rvalue immediate : rest) = (toAsm op True) rtarget rvalue immediate : _toAssembly rest
_toAssembly (Br cond reg label : rest) = (brAsm cond) reg label : _toAssembly rest
_toAssembly (Zero reg : rest) = XOR reg reg reg : _toAssembly rest
_toAssembly (Label a : rest) = Lbl a : _toAssembly rest
_toAssembly (ReadInt reg location : rest) = RD reg : _toAssembly [MemoryStore reg location] ++ _toAssembly rest
_toAssembly (ReadFloat reg location : rest) = RDR reg : _toAssembly [MemoryStore reg location] ++ _toAssembly rest
_toAssembly (_ : rest) = _toAssembly rest
_toAssembly [] = []


_codePrinter :: [Assembly] -> String
_codePrinter (WR reg : rest) = "WR r" ++ show reg ++ "\n" ++ _codePrinter rest
_codePrinter (WRR reg : rest) = "WRR r" ++ show reg ++ "\n" ++ _codePrinter rest
_codePrinter (WRS loc : rest) = "WRS " ++ show loc ++ "\n" ++ _codePrinter rest
_codePrinter (RD a : rest) = "RD r" ++ show a ++ "\n" ++ _codePrinter rest
_codePrinter (RDR a : rest) = "RDR r" ++ show a ++ "\n" ++ _codePrinter rest
_codePrinter (XOR a b c : rest) = "XOR r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (ADDI a b c : rest) = "ADDI r" ++ show a ++ " r" ++ show b ++ " " ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (SUBI a b c : rest) = "SUBI r" ++ show a ++ " r" ++ show b ++ " " ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (DIVI a b c : rest) = "DIVI r" ++ show a ++ " r" ++ show b ++ " " ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (MULI a b c : rest) = "MULI r" ++ show a ++ " r" ++ show b ++ " " ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (ADD a b c : rest) = "ADD r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (SUB a b c : rest) = "SUB r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (MUL a b c : rest) = "MUL r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (DIV a b c : rest) = "DIV r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (ADDR a b c : rest) = "ADDR r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (SUBR a b c : rest) = "SUBR r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (MULR a b c : rest) = "MULR r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (DIVR a b c : rest) = "DIVR r" ++ show a ++ " r" ++ show b ++ " r" ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (ITOR a b : rest) = "ITOR r" ++ show a ++ " r" ++ show b ++ "\n" ++ _codePrinter rest
_codePrinter (RTOI a b : rest) = "RTOI r" ++ show a ++ " r" ++ show b ++ "\n" ++ _codePrinter rest
_codePrinter (MOVIR a b : rest) = "MOVIR r" ++ show a ++ " " ++ show b ++ "\n" ++ _codePrinter rest
_codePrinter (BLTZR a b : rest) = "BLTZR r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BLTZ a b : rest) = "BLTZ r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BGEZR a b : rest) = "BGEZR r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BGEZ a b : rest) = "BGEZ r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BEQZR a b : rest) = "BEQZR r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BEQZ a b : rest) = "BEQZ r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BNEZR a b : rest) = "BNEZR r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (BNEZ a b : rest) = "BNEZ r" ++ show a ++ " " ++ b ++ "\n" ++ _codePrinter rest
_codePrinter (STORE a b c : rest) = "STORE r" ++ show a ++ " r" ++ show b ++ " " ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (LOAD a b c : rest) = "LOAD r" ++ show a ++ " r" ++ show b ++ " " ++ show c ++ "\n" ++ _codePrinter rest
_codePrinter (DATA a : rest) = "DATA " ++ show a ++ "\n" ++ _codePrinter rest
_codePrinter (Lbl a : rest) = a ++ ":\n" ++ _codePrinter rest
_codePrinter (HALT : rest) = "HALT\n" ++ _codePrinter rest
_codePrinter [] = ""
_codePrinter (a : rest) = error (show a)

toAssembly :: [IRForm] -> String
toAssembly = _codePrinter . _toAssembly

optToAssembly :: [IRForm] -> String
optToAssembly = _codePrinter . optAssembly . _toAssembly
