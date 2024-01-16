%{
  open Absyn
  (*open Exceptions*)
  (*open Lexing*)
%}
%token <int> CSTINT
%token <string> NAME
%token LPAR RPAR LBRAKE RBRAKE
%token PLUS MINUS TIMES EQ NEQ LT GT LTEQ GTEQ
%token PIPE NOT
%token COMMA DOT UP COLON EOF
%token IF THEN ELSE MATCH WITH
%token UNDERSCORE
%token LAMBDA
%token ARROW

/*Low precedence*/
%right ARROW
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS
%left TIMES 
%nonassoc NOT
/*High precedence*/

%start main
%type <Absyn.file> main
%%
main:
  stmt EOF     { File $1 }
;

stmt:
    expression { Anon $1 }
  | NAME COLON expression { Named($1, $3) }
;

expression:
  CSTINT { Constant $1 }
  | route { Route $1 }
  | expression PLUS expression { Binop("+", $1, $3) }
  | expression MINUS expression { Binop("-", $1, $3) }
  | expression TIMES expression { Binop("*", $1, $3) }
  | expression EQ expression { Binop("=", $1, $3) }
  | expression LT expression { Binop("<", $1, $3) }
  | expression GT expression { Binop(">", $1, $3) }
  | expression LTEQ expression { Binop("<=", $1, $3) }
  | expression GTEQ expression { Binop(">=", $1, $3) }
  | LBRAKE scope RBRAKE { Scope (List.rev $2) }
  | LAMBDA NAME ARROW expression { Func($2, $4) }
  | expression expression { Call($1, $2) }
  | IF expression THEN expression ELSE expression { If($2, $4, $6) }
  | MATCH expression WITH match_alts { Match($2, $4) }
  | LPAR expression RPAR { $2 }
;

match_alt:
  PIPE expression ARROW expression { ($2, $4) }
;

match_alts:
  match_alt { [$1] }
  | match_alt match_alts { $1 :: $2 }
;

route:
  | step { [$1] }
  | step DOT route { $1 :: $3 }
;

step: 
  NAME { Label $1 }
  | LBRAKE expression RBRAKE { Index $2 }
  | UP { OutOf }
;

scope:
  { [] }
  | stmt { [$1] }
  | stmt COMMA scope { $1::$3 }
;

