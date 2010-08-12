{
module Parser where
import Lexer
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
    'E' {TokenE}
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
    sliteral {TokenStringLiteral $$}
    identifier {TokenIdentifier $$}

$$

program : PROGRAM programname ';' block '.' {$2}

programname : identifier {$1}

block : declarationblock compoundstatement {Block $1 $2}

declarationblock :: {[Declaration]}
declarationblock : VAR declarations {$2}
                 | {-empty-} {[]}

declarations :: {[Declaration]}
declarations : idlist type ';' declarations {Declaration $1 $2 : $3}
             | idlist type ';' {Declaration $1 $2}

idlist :: {[Identifier]}
idlist : identifier {[$1]}
       | idlist ',' identifier {$1 ++ [$2]}

type :: [Type]
type : REAL {RealType}
     | INTEGER {IntType}

compoundstatement :: {[Statement]}
compoundstatement : BEGIN statements END {$1}

statements :: {[Statement]}
statements : statement ';' statements {$1 : $3}
           | {-Empty-} {[]}


statement :: {Statement}
statement : variable ':=' expression {Assign $1 $2}
          | READ '(' variable ')' {Read $3}
          | WRITE '(' expression ')' {Write $3}
          | WRITELN {WriteLn}
          | conditional {$1}
          | repeat {$1}

comparison :: {Comparison}
comparison : expresion relation expression {Comparison $2 $1 $3}

conditional:: {Statement}
conditional : IF comparison compoundstatement {If $1 $2}
            | IF comparison compoundstatement ELSE compoundstatement {IfElse $1 $2 $3}

repeat :: {Statement}
repeat : REPEAT compoundstatement UNTIL comparison {RepeatUntil $2 $1}

relation:: {Relation}
relation: '>' {RelGreater}
        | '>=' {RelGreaterEq}
        | '=' {RelEq}
        | '!=' {RelNeq}
        | '<=' {RelLessEq}
        | '<' {RelLess}


expression: firstFactor '+' factor
          | firstFactor '-' factor

factor: factor


unaryop: '+'
       | '-'
       | {-empty-}


variable: identifier

constant: iliteral '.' iliteral 'E' iliteral
        | iliteral
