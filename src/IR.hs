module IR where
import Data.Char
import Data.Maybe
import Data.List

import IRHelpers
import IRTypes
import ParserTypes

--convert an expression to ir form, put result in second integer arg
expToIr :: Expression -> [Declaration] -> Int -> Int -> ([IRForm],IRExpType)
expToIr (TermConstant (IntegerLiteral a)) decs offest resultReg = ([LoadImmediateInt resultReg a], TInt)
expToIr (TermConstant (RealLiteral a)) decs offset resultReg = ([LoadImmediateReal resultReg (a)],TReal)
expToIr (TermVar (id)) decs offset resultReg = ([MemoryLoad resultReg ((variableOffset decs id) + offset)], getType decs id)
expToIr (Op a child1 child2) decs offset resultReg = let
                                                        c1Pair = expToIr child1 decs offset resultReg
                                                        c2Pair = expToIr child2 decs offset (resultReg + 1)
                                                        richest = selectRichestType (snd c1Pair) (snd c2Pair)
                                                     in
                                                        (fst c1Pair ++
                                                         fst c2Pair ++
                                                         irForOp a resultReg richest (snd c1Pair) (snd c2Pair), richest)

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
_toAssembly (DoMath op a b c : rest) = toAsm op ++ " " ++ regToString a ++ " " ++ regToString b ++ " " ++ regToString c ++ "\n" ++ _toAssembly rest
_toAssembly [] = []


toAssembly :: [IRForm] -> String
toAssembly = _toAssembly
