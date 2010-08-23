module IR where
import ParserTypes

data IRForm = WriteInt {register :: Int} | LoadImmediate {register,value :: Int} | WriteLine deriving (Show, Read, Eq)

_toIrForm :: [Statement] -> [IRForm]
_toIrForm (WriteExp (TermConstant (IntegerLiteral a)) : rest) = LoadImmediate 0 a : WriteInt 0 : _toIrForm rest
_toIrForm (WriteLn : rest) = WriteLine : _toIrForm(rest)
_toIrForm (a : rest) = _toIrForm rest
_toIrForm [] = []

toIrForm :: Program -> [IRForm]
toIrForm (Program name (Block decs statements)) = _toIrForm statements

regToString :: Int -> String
regToString a = 'R' : show a

dataBegin :: [IRForm] -> Int
dataBegin (WriteInt a : rest) = 1 + (dataBegin rest)
dataBegin (LoadImmediate a b : rest) = 2 + (dataBegin rest)
dataBegin (WriteLine : rest) = 1 + (dataBegin rest)
dataBegin [] = 0

pseudoNewline = "__newline__" :: String

_toAssembly :: [IRForm] -> String
_toAssembly ((WriteInt {register = a}) : rest) = ";writing register r" ++ show a ++ "\nWR " ++ regToString a ++ "\n" ++ _toAssembly rest
_toAssembly ((LoadImmediate {register = reg, value = v}) : rest) = ";loading immediate " ++ show v ++ " into register r" ++ show reg
															++ "\nXOR " ++ regToString reg ++ " " ++ regToString reg ++ " " ++ regToString reg ++ "\n"
															++ "ADDI " ++ regToString reg ++ " " ++ regToString reg ++ " " ++ show v ++ "\n" ++ _toAssembly rest
_toAssembly (WriteLine : rest) = ";writing a newline" ++ "\nWRS " ++ pseudoNewline ++ "\n" ++ _toAssembly(rest)

--encode a newline for use in the program
_toAssembly [] = "HALT\nDATA 10\n DATA 0"

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
	        then repl ++ (replace (drop (length find) s) find repl)
			        else [head s] ++ (replace (tail s) find repl)


toAssembly :: [IRForm] -> String
toAssembly a = replace (_toAssembly a) pseudoNewline (show 0)
