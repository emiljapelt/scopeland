%{
  open Absyn
  (*open Lexing*)

%}
%token <int> CSTINT
%token <string> NAME
%token LPAR RPAR LBRAKE RBRAKE
%token PLUS MINUS TIMES EQ NEQ LT GT LTEQ GTEQ
%token PIPE AND EXCLAIM AT
%token COMMA DOT UP COLON EOF
%token IF THEN ELSE MATCH WITH IMPORT 
%token UNDERSCORE
%token ARROW LAMBDA

/*Low precedence*/
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left AND
%left PLUS MINUS
%left TIMES 
%right ELSE
/*High precedence*/

%start main
%type <Absyn.file> main
%%
main:
  stmt EOF     { File $1 }
;

stmt:
    expression_with_match { Anon $1 }
  | NAME COLON expression_with_match { Named($1, $3) }
  | EXCLAIM expression_with_match { Out $2 }
  | IMPORT route { Import $2 }
;

expression_with_match:
  MATCH expression WITH match_alts { Match($2, $4) }
  | expression { $1 }
;

expression:
  call { $1 }
  | simple_expression_and_route { $1 }
  | expression binop expression { Binop($2, $1, $3) }
  | IF expression THEN expression ELSE expression { If($2, $4, $6) }
;

simple_expression_and_route:
  | route { Route $1 }
  | simple_expression { $1 }
;

simple_expression:
  | CSTINT { Constant $1 }
  | LBRAKE scope RBRAKE { Scope (List.rev $2) }
  | LBRAKE LAMBDA args ARROW scope RBRAKE { Func($3, (List.rev $5)) }
  | LPAR expression_with_match RPAR { $2 }
;

scope:
  { [] }
  | stmt { [$1] }
  | stmt COMMA scope { $1::$3 }
;

route:
  | NAME route_inner { Label $1 :: $2}
  | simple_expression COLON route_inner { Index $1 :: $3}
  | UP route_inner { OutOf :: $2 }
  | AT route_inner { FullOut :: $2 }
;

route_inner:
  | step* { $1 }
;

step: 
  | DOT NAME { Label $2 }
  | DOT simple_expression { Index $2 }
  | DOT UP { OutOf }
  | DOT AT { FullOut }
;

%inline binop:
  | PLUS    { ("+") }
  | MINUS   { ("-") }
  | TIMES   { ("*") } 
  | EQ      { ("=") }
  | NEQ     { ("!=") }
  | LT      { ("<") }
  | GT      { (">") }
  | LTEQ    { ("<=") }
  | GTEQ    { (">=") }
  | AND     { ("&") }
;

args:
  NAME { [$1] }
  | NAME args { $1 :: $2 }
;

call:
  simple_expression_and_route simple_expression_and_route { Call($1,$2) }
  | call simple_expression_and_route { Call($1,$2) }
;

pattern:
  simple_pattern { $1 }
  | pattern AND simple_pattern { ScopeList($1,$3) }
;

tuple_pattern:
  pattern { [$1] }
  | pattern COMMA tuple_pattern { $1::$3 }
;

simple_pattern:
  | CSTINT { Concrete $1 }
  | NAME { Name $1 }
  | LBRAKE RBRAKE { Empty }
  | UNDERSCORE { Any }
  | LPAR pattern RPAR { $2 }
  | LBRAKE tuple_pattern RBRAKE { ScopeTuple $2 }
;

match_alt:
  PIPE pattern ARROW expression { ($2, $4) }
;

match_alts:
  | match_alt { [$1] }
  | match_alt match_alts { $1 :: $2 }
;
