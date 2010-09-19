module Optimiser where
import Semantics
import ParserTypes

multiplyByOne :: Expression -> Expression
multiplyByOne (Op Multiply a (TermConstant (IntegerLiteral 1))) = multiplyByOne a
multiplyByOne (Op Multiply (TermConstant (IntegerLiteral 1)) a) = multiplyByOne a
multiplyByOne (Op Multiply a (TermConstant (RealLiteral 1))) = multiplyByOne a
multiplyByOne (Op Multiply (TermConstant (RealLiteral 1)) a) = multiplyByOne a
multiplyByOne (Op a b c) = Op a (multiplyByOne b) (multiplyByOne c)
multiplyByOne (TermConstant a) = TermConstant a
multiplyByOne (TermVar a) = TermVar a

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
optTrans = multiplyByOne . addZero

parseOptimise :: Program -> Program
parseOptimise prog = let p = transformExpressions optTrans prog in if (p == prog) then p else parseOptimise p


