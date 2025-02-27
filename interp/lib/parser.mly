%{
  open Absyn
  (*open Lexing*)

%}
%token <int> CSTINT
%token <string> CSTSTR
%token <string> NAME
%token LPAR RPAR LBRAKE RBRAKE
%token PLUS MINUS TIMES EQ NEQ LT GT LTEQ GTEQ
%token PIPE AND EXCLAIM AT
%token COMMA DOT UP COLON EOF
%token IF THEN ELSE MATCH WITH IMPORT STRING INT
%token UNDERSCORE INTO
%token ARROW LAMBDA

/*Low precedence*/
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left AND
%left PLUS MINUS
%left TIMES 
%right ELSE 
%left INTO
/*High precedence*/

%start main
%type <Absyn.file> main
%%
main:
  stmt EOF     { File $1 }
  | stmt COMMA scope EOF  { File (Anon(Scope(List.rev($1::$3)))) }
;

stmt:
    expression_with_match { Anon $1 }
  | NAME COLON expression_with_match { Named($1, $3) }
  | EXCLAIM expression_with_match { Out $2 }
  | IMPORT dot_start_route { Import $2 }
;

expression_with_match:
  MATCH expression WITH match_alts { Match($2, $4) }
  | expression { $1 }
;

expression:
  call { $1 }
  | simple_expression { $1 }
  | expression binop expression { Binop($2, $1, $3) }
  | IF expression THEN expression ELSE expression { If($2, $4, $6) }
  | expression INTO expression  { Call ($3,$1) }
  | dot_start_route { Route $1 }
;

simple_expression_and_route:
  | route { Route $1 }
  | simple_expression { $1 }
;

simple_expression:
  | CSTINT { Integer $1 }
  | CSTSTR { String $1 }
  | LBRAKE scope RBRAKE { Scope (List.rev $2) }
  | LBRAKE LAMBDA args ARROW scope RBRAKE { Func($3, (List.rev $5)) }
  | LPAR expression_with_match RPAR { $2 }
;

scope:
  { [] }
  | stmt { [$1] }
  | stmt COMMA scope { $1::$3 }
;

dot_start_route:
  | DOT route_inner_no_dot { $2 }
  | route { $1 }
;

route:
  | NAME route_inner { Label $1 :: $2 }
  | UP route_inner { OutOf :: $2 }
  | AT route_inner { FullOut :: $2 }
  | UNDERSCORE route_inner { Index(Integer(-1)) :: $2 }
;

route_inner_no_dot:
  | {[]}
  | step route_inner { $1 :: $2 }
;

route_inner:
  | {[]}
  | DOT step route_inner { $2 :: $3 }
;

step: 
  | NAME { Label $1 }
  | simple_expression { Index $1 }
  | UP { OutOf }
  | AT { FullOut }
  | UNDERSCORE { Index(Integer(-1)) }
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
  | CSTINT { IntegerPat $1 }
  | CSTSTR { StringPat $1 }
  | INT { IntegerTypePat }
  | STRING { StringTypePat }
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
