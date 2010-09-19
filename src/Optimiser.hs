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


optTrans :: Expression -> Expression
optTrans a = a

parseOptimise :: Program -> Program
parseOptimise prog = let p = transformExpressions optTrans prog in if p == prog then p else parseOptimise p


