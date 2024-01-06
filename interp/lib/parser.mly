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
%token IF THEN ELSE
%token UNDERSCORE
%token ARROW

/*Low precedence*/
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS
%left TIMES 
%left ARROW
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
  | expression LT expression { Binop("<", $1, $3) }
  | LBRAKE scope RBRAKE { Scope (List.rev $2, NullScope) }
  | NAME ARROW expression { Func($1, $3) }
  | route expression { Call($1, $2) }
  | IF expression THEN expression ELSE expression { If($2, $4, $6) }
  | LPAR expression RPAR { $2 }
;

route:
  NAME DOT route { InTo($1,$3) }
  | NAME { Label $1 }
  | LBRAKE expression RBRAKE { Index $2 }
  | UP DOT route { OutOf $3 }
;

scope:
  { [] }
  | stmt { [$1] }
  | stmt COMMA scope { $1::$3 }
;

