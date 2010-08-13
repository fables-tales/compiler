{
module Parser where
import System.IO.Unsafe
import IO
import Lexer
import ParserTypes
}

%name camleParser
%tokentype { Token }
%error { parseError }


%token
    ',' {TokenComma}
    PROGRAM {TokenProgram}
    '+' {TokenPlus}
    '-' {TokenMinus}
    '*' {TokenMul}
    '/' {TokenDiv}
    INTEGER {TokenINTEGER}
    REAL {TokenREAL}
    '(' {TokenOb}
    ')' {TokenCb}
    '.' {TokenDot}
    ';' {TokenSemiColon}
    VAR {TokenVar}
    ':=' {TokenAssign}
    READ {TokenRead}
    WRITE {TokenWrite}
    WRITELN {TokenWriteLn}
    '>' {TokenGreater}
    '<' {TokenLess}
    '>=' {TokenGreaterEq}
    '<=' {TokenLessEq}
    '=' {TokenEq}
    '!=' {TokenNeq}
    BEGIN {TokenBegin}
    END {TokenEnd}
    REPEAT {TokenRepeat}
    UNTIL {TokenUntil}
    IF {TokenIf}
    ELSE {TokenElse}

    iliteral {TokenIntLiteral $$}
    rliteral {TokenRealLiteral $$}
    sliteral {TokenStringLiteral $$}
    identifier {TokenIdentifier $$}

%left '/' '*' '+' '-'
%%
program :: {Program}
program : PROGRAM programname ';' block '.' {Program $2 $4}

programname :: {String}
programname : identifier {$1}

variable :: {Identifier}
variable: identifier {VarIdentifier $1}

block :: {Block}
block : declarationblock compoundstatement {Block $1 $2}

declarationblock :: {[Declaration]}
declarationblock : VAR declarations {$2}
                 | {-empty-} {[]}

declarations :: {[Declaration]}
declarations : idlist type ';' declarations {Declaration $1 $2 : $4}
             | idlist type ';' {[Declaration $1 $2]}

idlist :: {[Identifier]}
idlist : identifier {[VarIdentifier $1]}
       | idlist ',' identifier {$1 ++ [VarIdentifier $3]}

type :: {Type}
type : REAL {RealType}
     | INTEGER {IntType}

compoundstatement :: {[Statement]}
compoundstatement : BEGIN statements END {$2}

statements :: {[Statement]}
statements : statement ';' statements {$1 : $3}
           | {-Empty-} {[]}


statement :: {Statement}
statement : variable ':=' expression {Assign $1 $3}
          | READ '(' variable ')' {Read $3}
          | WRITE '(' expression ')' {WriteExp $3}
          | WRITE '(' sliteral ')' {WriteS $3}
          | WRITELN {WriteLn}
          | conditional {$1}
          | repeat {$1}

comparison :: {Comparison}
comparison : expression relation expression {Comparison $2 $1 $3}

conditional :: {Statement}
conditional : IF comparison compoundstatement {If $2 $3}
            | IF comparison compoundstatement ELSE compoundstatement {IfElse $2 $3 $5}

repeat :: {Statement}
repeat : REPEAT compoundstatement UNTIL comparison {RepeatUntil $4 $2}

relation :: {Relation}
relation: '>' {RelGreater}
        | '>=' {RelGreaterEq}
        | '=' {RelEq}
        | '!=' {RelNeq}
        | '<=' {RelLessEq}
        | '<' {RelLess}

expression :: {Expression}
expression: prefix '+' restExp {Add $1 $3}
          | prefix '-' restExp {Subtract $1 $3}
          | prefix '*' restExp {Multiply $1 $3}
          | prefix '/' restExp {Divide $1 $3}
          | prefix {$1}

restExp :: {Expression}
restExp: term {$1}
       | '(' expression ')' {$2}
       | restExp '+' restExp {Add $1 $3}
       | restExp '-' restExp {Subtract $1 $3}
       | restExp '*' restExp {Multiply $1 $3}
       | restExp '/' restExp {Divide $1 $3}

term :: {Expression}
term: variable {TermVar $1}
    | constant {TermConstant $1}

prefix :: {Expression}
prefix: unaryop term {if $1 == UnaryPlus then $2 else _negate $2 }
      | unaryop '(' expression ')' {if $1 == UnaryPlus then $3 else _negate $3}

unaryop :: {UnaryOp}
unaryop: '+' {UnaryPlus}
       | '-' {UnaryMinus}
       | {-empty-} {UnaryPlus}


constant :: {NumberLiteral}
constant: rliteral {RealLiteral $1}
        | iliteral {IntegerLiteral $1}
