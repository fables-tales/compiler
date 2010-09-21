module Optimiser where
import Semantics
import ParserTypes
import IRTypes

--strength reduction on multiply by one
multiplyByOne :: Expression -> Expression
multiplyByOne (Op Multiply a (TermConstant (IntegerLiteral 1))) = multiplyByOne a
multiplyByOne (Op Multiply (TermConstant (IntegerLiteral 1)) a) = multiplyByOne a
multiplyByOne (Op Multiply a (TermConstant (RealLiteral 1))) = multiplyByOne a
multiplyByOne (Op Multiply (TermConstant (RealLiteral 1)) a) = multiplyByOne a
multiplyByOne (Op a b c) = Op a (multiplyByOne b) (multiplyByOne c)
multiplyByOne (TermConstant a) = TermConstant a
multiplyByOne (TermVar a) = TermVar a

--multiplication by zero is equal to zero
multiplyByZero :: Expression -> Expression
multiplyByZero (Op Multiply a (TermConstant (IntegerLiteral 0))) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op Multiply (TermConstant (IntegerLiteral 0)) a) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op Multiply a (TermConstant (RealLiteral 0))) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op Multiply (TermConstant (RealLiteral 0)) a) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op a b c) = Op a (multiplyByZero b) (multiplyByZero c)
multiplyByZero (TermConstant a) = TermConstant a
multiplyByZero (TermVar a) = TermVar a

subtractSame :: Expression -> Expression
subtractSame (Op Subtract a b) | a == b = TermConstant (IntegerLiteral 0)
subtractSame (Op a b c) = Op a (subtractSame b) (subtractSame c)
subtractSame (TermConstant a) = TermConstant a
subtractSame (TermVar a) = TermVar a

--used to test the tests, breaks expressions
breakIt :: Expression -> Expression
breakIt (Op a b c) = Op Add (breakIt b) (breakIt c)
breakIt (TermConstant a) = TermConstant a
breakIt (TermVar a) = TermVar a

--adding zero to x is the same as x
addZero :: Expression -> Expression
addZero (Op Add a (TermConstant (IntegerLiteral 0))) = addZero a
addZero (Op Add (TermConstant (IntegerLiteral 0)) a) = addZero a
addZero (Op Add a (TermConstant (RealLiteral 0))) = addZero a
addZero (Op Add (TermConstant (RealLiteral 0)) a) = addZero a
addZero (Op a b c) = Op a (addZero b) (addZero c)
addZero (TermConstant a) = TermConstant a
addZero (TermVar a) = TermVar a

optTrans :: Expression -> Expression
optTrans = multiplyByOne . addZero . multiplyByZero . subtractSame

parseOptimise :: Program -> Program
parseOptimise prog = let p = transformExpressions optTrans prog in if p == prog then p else parseOptimise p

--both int zero and floating zero are all zero bits, so just zero the reg
zeroOpt :: [IRForm] -> [IRForm]
zeroOpt (LoadImmediateInt reg 0 : rest) = Zero reg : zeroOpt rest
zeroOpt (LoadImmediateReal reg 0.0 : rest) = Zero reg : zeroOpt rest
zeroOpt (a : rest) = a : zeroOpt rest
zeroOpt [] = []

--if we're loading the same var twice, don't do the load twice
sameVar :: [IRForm] -> [IRForm]
sameVar (MemoryLoad reg1 location1 :
        MemoryLoad reg2 location2 :
        DoMath op r1 r2 r3 : rest) | reg1 + 1 == reg2 && r2 == reg1
                                     && location1 == location2 = MemoryLoad reg1 location1 :
                                                                    DoMath op r1 r2 r2
                                                                    : sameVar rest
sameVar (a : rest) = a : sameVar rest
sameVar [] = []

dualLoad :: [IRForm] -> [IRForm]
dualLoad (LoadImmediateInt reg1 value1
            : LoadImmediateInt reg2 value2 :
            rest) = LoadImmediateInt reg1 value1 :
                    DoMathImmediate AddInt reg2 reg1 (value2 - value1)
                    : dualLoad rest

dualLoad (a : rest) = a : dualLoad rest
dualLoad [] = []

optIrTrans :: [IRForm] -> [IRForm]
optIrTrans = zeroOpt . sameVar . dualLoad

optimiseIr :: [IRForm] -> [IRForm]
optimiseIr ir = let trans = optIrTrans ir in if trans == ir then trans else optimiseIr trans
