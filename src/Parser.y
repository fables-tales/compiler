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

type :: {Type}
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
          | WRITE '(' expression ')' {WriteExp $3}
          | WRITE '(' sliteral ')' {WriteS $3}
          | WRITELN {WriteLn}
          | conditional {$1}
          | repeat {$1}

comparison :: {Comparison}
comparison : expression relation expression {Comparison $2 $1 $3}

conditional :: {Statement}
conditional : IF comparison compoundstatement {If $1 $2}
            | IF comparison compoundstatement ELSE compoundstatement {IfElse $1 $2 $3}

repeat :: {Statement}
repeat : REPEAT compoundstatement UNTIL comparison {RepeatUntil $2 $1}

relation :: {Relation}
relation: '>' {RelGreater}
        | '>=' {RelGreaterEq}
        | '=' {RelEq}
        | '!=' {RelNeq}
        | '<=' {RelLessEq}
        | '<' {RelLess}

expression :: {Expression}
expression: prefix '+' restExp {Add $1 $2}
          | prefix '-' restExp {Subtract $1 $2}
          | prefix '*' restExp {Multiply $1 $2}
          | prefix '/' restExp {Divide $1 $2}
          | prefix {$1}
          | unaryop '(' expression ')' {if $1 == UnaryPlus then $3 else negate $2}

restExp :: {Expression}
restExp: term {$1}
       | '(' expression ')' {$1}
       | restExp '+' restExp {Add $1 $2}
       | restExp '-' restExp {Subtract $1 $2}
       | restExp '*' restExp {Multiply $1 $2}
       | restExp '/' restExp {Divide $1 $2}

term :: {Expression}
term: variable {TermVar $1}
    | constant {TermConst $1}

prefix :: {Expression}
prefix: unaryop term {if $1 == UnaryPlus then $2 else negate $2}

unaryop: '+' {UnaryPlus}
       | '-' {UnaryMinus}
       | {-empty-} {UnaryPlus}


variable: identifier {VarIdentifier $1}

constant: rliteral {RealLiteral $1}
        | iliteral {IntegerLiteral $1}
