{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      '.'             { Dot }
      match           { Match }
      zero            { Zero }
      suc             { Suc }
      '\\'            { Backslash }
      int             { Num $$ }
      '('             { LParen }
      ')'             { RParen }
      '='             { Assign }
      ':'             { Colon }
      "->"            { Arrow }
      nat             { Nat }
      main            { Main }
      var             { ID $$ }

%nonassoc ABS
%nonassoc MATCH
%right SUC
%left '.'
%right "->"
%%

Prog : Fun Prog  { Decl $1 $2 }
     | main ':' Type main '=' Exp { MainF $3 $6 } 

Exp : var            { Var $1 }
    | int            { NumE $1 }
    | suc Exp       %prec SUC { SucE $2 }
    | '\\' var ':' Type Exp %prec ABS  { Abs $2 $4 $5 }
    | Exp '.' Exp    { App $1 $3 }
    | match Exp zero "->" Exp suc var "->" Exp %prec MATCH { MatchE $2 $5 ($7, $9) }
    | '(' Exp ')'    { BracketE $2 }

Type : nat             { NatT }
     | Type "->" Type  { ArrowT $1 $3 }
     | '(' Type ')'    { BracketT $2 }

Fun : var ':' Type var '=' Exp { Fun $1 $3 $4 $6 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp 
    = Var String
    | NumE Int 
    | SucE Exp
    | Abs String Type Exp
    | App Exp Exp
    | MatchE Exp Exp (String, Exp)
    | BracketE Exp
    deriving (Eq, Show)

data Type 
    = NatT
    | ArrowT Type Type
    | BracketT Type
    deriving (Eq, Show)

data Fun 
    = Fun String Type String Exp
    deriving (Eq, Show)

data Prog 
    = Decl Fun Prog 
    | MainF Type Exp      
    deriving (Eq, Show)
}