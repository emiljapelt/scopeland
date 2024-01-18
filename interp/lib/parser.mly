%{
  open Absyn
  open Exceptions
  (*open Lexing*)

  let expr_list_to_call exprs =
    let rec aux es acc = match es with
      | [] -> acc
      | h::t -> aux t (Call(acc, h)) 
    in 
    match exprs with
    | e1::e2::t -> aux t (Call(e1,e2))
    | _ -> raise_failure "Call of bad format"
%}
%token <int> CSTINT
%token <string> NAME
%token LPAR RPAR LBRAKE RBRAKE
%token PLUS MINUS TIMES EQ NEQ LT GT LTEQ GTEQ
%token PIPE
%token COMMA DOT UP COLON EOF
%token IF THEN ELSE MATCH WITH
%token UNDERSCORE
%token ARROW LAMBDA

/*Low precedence*/
%left ELSE
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS
%left TIMES 
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
;

expression_with_match:
  MATCH expression WITH match_alts { Match($2, $4) }
  | expression { $1 }
;

expression:
  call { expr_list_to_call $1 }
  | simple_expression { $1 }
  | expression binop expression { Binop($2, $1, $3) }
;

simple_expression:
  CSTINT { Constant $1 }
  | route { Route $1 }
  | LBRAKE scope RBRAKE { Scope (List.rev $2) }
  | LBRAKE LAMBDA args ARROW scope RBRAKE { Func($3, (List.rev $5)) }
  | IF expression THEN expression ELSE expression { If($2, $4, $6) }
  | LPAR expression_with_match RPAR { $2 }
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
;

args:
  NAME { [$1] }
  | NAME args { $1 :: $2 }
;

call:
  simple_expression simple_expression { [$1;$2] }
  | simple_expression call { $1 :: $2 }
;

pattern:
  CSTINT { Concrete $1 }
  | UNDERSCORE { Any }
;

match_alt:
  PIPE pattern ARROW expression { ($2, $4) }
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
  | LBRAKE COLON expression RBRAKE { Index $3 }
  | UP { OutOf }
;

scope:
  { [] }
  | stmt { [$1] }
  | stmt COMMA scope { $1::$3 }
;

