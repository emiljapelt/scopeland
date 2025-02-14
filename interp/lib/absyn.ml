
type expression =
    | Integer of int
    | String of string
    | Route of route
    | Binop of string * expression * expression
    | Scope of stmt list
    | Func of string list * stmt list
    | If of expression * expression * expression
    | Call of expression * expression
    | Match of expression * (pattern * expression) list

and pattern =
    | IntegerPat of int
    | StringPat of string
    | IntegerTypePat
    | StringTypePat
    | Name of string
    | ScopeList of pattern * pattern
    | ScopeTuple of pattern list
    | Empty
    | Any 

and stmt =
    | Named of string * expression
    | Anon of expression
    | Out of expression
    | Import of route

and step = 
    | Label of string
    | Index of expression
    | OutOf
    | FullOut
    
and route = step list

and scope = 
    | NullScope of string
    | InnerScope of (string option * value) list * scope * scope * string

and value =
    | IntegerVal of int * string option
    | StringVal of string * string option
    | Closure of (string list) * (stmt list) * ((string * value) list) * scope * string option
    | ScopeVal of scope * string option

and file = 
    | File of stmt

let value_name v = match v with
    | IntegerVal(_,n)
    | StringVal(_,n)
    | Closure(_,_,_,_,n)
    | ScopeVal(_,n) -> n

and remove_name value = match value with
    | IntegerVal(i,_) -> IntegerVal(i,None)
    | StringVal(s,_) -> StringVal(s,None)
    | Closure(a,b,c,d,_) -> Closure(a,b,c,d,None)
    | ScopeVal(scp,_) -> ScopeVal(scp,None)

and scope_dir scope = match scope with
    | NullScope dir 
    | InnerScope(_,_,_,dir) -> dir

let is_null_scope scope = match scope with
    | NullScope _ -> true | _ -> false

let rec route_string route =
    match route with
    | [] -> ""
    | h::[] -> (match h with 
        | Label(ln) -> ln
        | Index(expr) -> "["^expression_string expr^"]"
        | OutOf -> "^"
        | FullOut -> "@"
    )
    | h::t -> (match h with
        | Label(ln) -> ln ^ "." ^ route_string t
        | Index(e) -> "["^expression_string e^"]" ^ "." ^ route_string t
        | OutOf -> "^." ^ route_string t
        | FullOut -> "@." ^ route_string t
    )

and value_string value = match value with
    | IntegerVal(i,Some n) -> n ^ ": " ^ string_of_int i
    | IntegerVal(i,None) -> string_of_int i
    | StringVal(s,Some n) -> n ^ ": " ^ s
    | StringVal(s,None) -> s
    | Closure(args_n,body,_,_,Some n) -> n ^ ": " ^ String.concat " " args_n ^ " -> " ^ expression_string (Scope body)
    | Closure(args_n,body,_,_,None) -> String.concat " " args_n ^ " -> " ^ expression_string (Scope body)
    | ScopeVal(NullScope _,_) -> "null scope"
    | ScopeVal(InnerScope(vals,_,_,_),_) -> (
        let content = List.map (fun (_,v) -> value_string v) vals(*List.rev vals*) in
        "[" ^ (String.concat ", " content) ^ "]"
    )


and expression_string expr = match expr with
    | Integer i -> string_of_int i
    | String s -> "\"" ^ s ^ "\""
    | Route rt -> route_string rt
    | Binop(op, expr1, expr2) -> "(" ^ expression_string expr1 ^ " " ^ op ^ " " ^ expression_string expr2 ^ ")"
    | Scope(stmts) -> (
        let content = List.map (fun stmt -> match stmt with 
            | Named(n,e) -> n^": "^expression_string e 
            | Anon e -> expression_string e
            | _ -> ""
        ) stmts in
        "[" ^ (String.concat ", " content) ^ "]"
    ) 
    | Func(args, stmts) -> (String.concat " " args) ^ " -> " ^ expression_string (Scope stmts)
    | If(cond,expr1,expr2) -> "if " ^ expression_string cond ^ " then " ^ expression_string expr1 ^ " else " ^ expression_string expr2
    | Call(func, expr) -> expression_string func ^ " " ^ expression_string expr
    | Match(expr, _) -> "match " ^ expression_string expr ^ " with"
