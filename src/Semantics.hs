module Semantics where
import List
import Maybe
import Data.Char

import ParserTypes

--unrwap an identifier
unwrapIdentifier :: Identifier -> String
unwrapIdentifier (VarIdentifier a) = map toLower a

--associate a type with an identifier
addType :: Type -> Identifier -> (String, Type)
addType a b = (unwrapIdentifier b, a)

--builds the symbol table from an array of declarations
buildSymbolTable :: [Declaration] -> [(String, Type)]
buildSymbolTable [] = []
buildSymbolTable (Declaration a t : rest) = [addType t x | x <- a] ++ buildSymbolTable rest

notInTable :: [(String, Type)] -> Identifier -> Bool
notInTable table id = find ((== unwrapIdentifier id) . fst) table == Nothing

--determines whether an expression contains a variable that isn't in the symbol table
expBadSymbol :: [(String, Type)] -> Expression -> Bool
expBadSymbol table (Op _ a b) = expBadSymbol table a || expBadSymbol table b
expBadSymbol table (TermConstant lit) = False
expBadSymbol table (TermVar id) = notInTable table id

--determines whether an expression contains a constant that isn't in the symbol table
cmpBadSymbol :: [(String, Type)] -> Comparison -> Bool
cmpBadSymbol table (Comparison rel exp1 exp2) = expBadSymbol table exp1 || expBadSymbol table exp2

--figures out if an array of statements references a symbol that doesn't exist in the symbol table
--returns true if an identifier exists which isn't in the symbol table, otherwise returns false
--
--this function looks slightly weird at first, originally it looked like if (conditions) true else tableCheck table rest,
--that's not necessary because 1) that's boolean equivalent to conditions OR tableCheck and 2) the short circuit will still
--happen on some platforms, meaning it should do the same early return/even sooner early return
--
--I like this form more because it's just a boolean expression instead of an ifelse
tableCheck :: [(String, Type)] -> [Statement] -> Bool
tableCheck table (Assign id exp : rest) = notInTable table id || expBadSymbol table exp || tableCheck table rest
tableCheck table (Read id : rest) = notInTable table id || tableCheck table rest
tableCheck table (WriteExp exp : rest) = expBadSymbol table exp || tableCheck table rest
tableCheck table (WriteS string : rest) = tableCheck table rest
tableCheck table (WriteLn : rest) = tableCheck table rest
tableCheck table (If comp statements : rest) = cmpBadSymbol table comp || tableCheck table statements || tableCheck table rest
tableCheck table (IfElse comp s1 s2 : rest) = cmpBadSymbol table comp || tableCheck table s1 || tableCheck table s2 || tableCheck table rest
tableCheck table (RepeatUntil comp statements : rest) = cmpBadSymbol table comp || tableCheck table statements || tableCheck table rest
tableCheck table [] = False

--verify symbols over the program
verifySymbols :: Program -> Bool
verifySymbols (Program pname (Block decs statements)) = not (tableCheck (buildSymbolTable decs) statements)

--check if an Identifier is a real or string identifier
realIdentifier :: [(String, Type)] -> Identifier -> Bool
realIdentifier table id = (snd . fromJust) (find ((== unwrapIdentifier id) . fst) table) == RealType

--replaces Mul -1 constant with -constant
_foldNegation :: Expression -> Expression
_foldNegation (Op Multiply (TermConstant (IntegerLiteral (-1))) (TermConstant (RealLiteral a))) = TermConstant (RealLiteral (-a))
_foldNegation (Op Multiply (TermConstant (IntegerLiteral (-1))) (TermConstant (IntegerLiteral a))) = TermConstant (IntegerLiteral (-a))
_foldNegation (Op a e1 e2) = Op a (_foldNegation e1) (_foldNegation e2)
_foldNegation (TermVar a) = TermVar a
_foldNegation (TermConstant a) = TermConstant a

--verifies that we can specify integers in 32 bits
--note: returns true if there is an error
_verifyConstants :: Expression -> Bool
_verifyConstants (TermConstant (IntegerLiteral a)) = a > (2^31-1) || a < (-(2^31))
_verifyConstants (TermVar a) = False
_verifyConstants (Op a e1 e2) = _verifyConstants e1 || _verifyConstants e2
_verifyConstants (TermConstant (RealLiteral a)) = False

--apply an expression transform to comparison
_transformComp :: (Expression -> Expression) -> Comparison -> Comparison
_transformComp f (Comparison rel e1 e2) = Comparison rel (f e1) (f e2)

--transforms all expressions using the transformation function
_transformExpressions :: (Expression -> Expression) -> [Statement] -> [Statement]
_transformExpressions f (Assign id exp : rest) = Assign id (f exp) : _transformExpressions f rest
_transformExpressions f (WriteExp e : rest) = WriteExp (f e) : _transformExpressions f rest
_transformExpressions f (If cmp statements : rest) = If (_transformComp f cmp) (_transformExpressions f statements) : _transformExpressions f rest
_transformExpressions f (IfElse cmp s1 s2 : rest) = IfElse (_transformComp f cmp) (_transformExpressions f s1) (_transformExpressions f s2) : _transformExpressions f rest
_transformExpressions f (RepeatUntil cmp s : rest) = RepeatUntil (_transformComp f cmp) (_transformExpressions f s) : _transformExpressions f rest
_transformExpressions f (a : rest) = a : _transformExpressions f rest
_transformExpressions f [] = []

transformExpressions :: (Expression -> Expression) -> Program -> Program
transformExpressions f (Program pname (Block decs statements)) = Program pname (Block decs (_transformExpressions f statements))

_mapCmp :: (Expression -> a) -> Comparison -> [a]
_mapCmp f (Comparison rel e1 e2) = [f e1,f e2]

--maps function over all the expressions in the program, no tree heirarchy is maintained
--useful for analytics over the entire program
_mapOverExpressions :: (Expression -> a) -> [Statement] -> [a]
_mapOverExpressions f (Assign id exp : rest) = f exp : _mapOverExpressions f rest
_mapOverExpressions f (WriteExp e : rest) = f e : _mapOverExpressions f rest
_mapOverExpressions f (If cmp statements : rest) = _mapCmp f cmp ++ _mapOverExpressions f statements ++ _mapOverExpressions f rest
_mapOverExpressions f (IfElse cmp s1 s2 : rest)  = _mapCmp f cmp ++ _mapOverExpressions f s1 ++ _mapOverExpressions f s2 ++ _mapOverExpressions f rest
_mapOverExpressions f (RepeatUntil cmp statements : rest) = _mapCmp f cmp ++ _mapOverExpressions f statements ++ _mapOverExpressions f rest
_mapOverExpressions f (a : rest) = _mapOverExpressions f rest
_mapOverExpressions f [] = []

_defaultTransform :: [Statement] -> [Statement]
_defaultTransform = _transformExpressions  _foldNegation

defaultTransform :: [Statement] -> [Statement]
defaultTransform tree = let nextTree = _defaultTransform tree in if nextTree == tree then tree else defaultTransform nextTree

--returns true if the semantics of the program are valid, false otherwise
verifySemantics :: Program -> (Bool,Program)
verifySemantics (Program pname (Block decs statements)) = let transformedStatements = defaultTransform statements in
														      (not (tableCheck (buildSymbolTable decs) transformedStatements) &&
															  (not . or) (_mapOverExpressions _verifyConstants transformedStatements),
															  Program pname (Block decs transformedStatements))
