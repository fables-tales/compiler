module Optimiser where
import Semantics
import ParserTypes
import IRTypes

multiplyByOne :: Expression -> Expression
multiplyByOne (Op Multiply a (TermConstant (IntegerLiteral 1))) = multiplyByOne a
multiplyByOne (Op Multiply (TermConstant (IntegerLiteral 1)) a) = multiplyByOne a
multiplyByOne (Op Multiply a (TermConstant (RealLiteral 1))) = multiplyByOne a
multiplyByOne (Op Multiply (TermConstant (RealLiteral 1)) a) = multiplyByOne a
multiplyByOne (Op a b c) = Op a (multiplyByOne b) (multiplyByOne c)
multiplyByOne (TermConstant a) = TermConstant a
multiplyByOne (TermVar a) = TermVar a

multiplyByZero :: Expression -> Expression
multiplyByZero (Op Multiply a (TermConstant (IntegerLiteral 0))) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op Multiply (TermConstant (IntegerLiteral 0)) a) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op Multiply a (TermConstant (RealLiteral 0))) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op Multiply (TermConstant (RealLiteral 0)) a) = TermConstant (IntegerLiteral 0)
multiplyByZero (Op a b c) = Op a (multiplyByZero b) (multiplyByZero c)
multiplyByZero (TermConstant a) = TermConstant a
multiplyByZero (TermVar a) = TermVar a

breakIt :: Expression -> Expression
breakIt (Op a b c) = Op Add (breakIt b) (breakIt c)
breakIt (TermConstant a) = TermConstant a
breakIt (TermVar a) = TermVar a

addZero :: Expression -> Expression
addZero (Op Add a (TermConstant (IntegerLiteral 0))) = addZero a
addZero (Op Add (TermConstant (IntegerLiteral 0)) a) = addZero a
addZero (Op Add a (TermConstant (RealLiteral 0))) = addZero a
addZero (Op Add (TermConstant (RealLiteral 0)) a) = addZero a
addZero (Op a b c) = Op a (addZero b) (addZero c)
addZero (TermConstant a) = TermConstant a
addZero (TermVar a) = TermVar a

optTrans :: Expression -> Expression
optTrans = multiplyByOne . addZero . multiplyByZero

parseOptimise :: Program -> Program
parseOptimise prog = let p = transformExpressions optTrans prog in if (p == prog) then p else parseOptimise p

zeroOpt :: [IRForm] -> [IRForm]
zeroOpt (LoadImmediateInt reg 0 : rest) = Zero reg : zeroOpt rest
zeroOpt (LoadImmediateReal reg 0.0 : rest) = Zero reg : zeroOpt rest
zeroOpt (a : rest) = a : zeroOpt rest
zeroOpt [] = []

optIrTrans :: [IRForm] -> [IRForm]
optIrTrans = zeroOpt
