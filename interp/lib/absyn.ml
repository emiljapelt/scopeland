
type expression =
    | Constant of int
    | Route of route
    | Binop of string * expression * expression
    | Scope of stmt list
    | Func of string list * stmt list
    | If of expression * expression * expression
    | Call of expression * expression
    | Match of expression * (pattern * expression) list

and pattern =
    | Concrete of int
    | Name of string
    | ScopePat of pattern * pattern
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
    | NullScope
    | InnerScope of (string option * value) list * scope

and value =
    | Value of int * string option
    | Closure of (string list) * (stmt list) * ((string * value) list) * scope * string option
    | ScopeVal of scope * string option

and file = 
    | File of stmt

let value_name v = match v with
    | Value(_,n)
    | Closure(_,_,_,_,n)
    | ScopeVal(_,n) -> n

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
    | Value(i,Some n) -> n ^ ": " ^ string_of_int i
    | Value(i,None) -> string_of_int i
    | Closure(args_n,body,_,_,Some n) -> n ^ ": " ^ String.concat " " args_n ^ " -> " ^ expression_string (Scope body)
    | Closure(args_n,body,_,_,None) -> String.concat " " args_n ^ " -> " ^ expression_string (Scope body)
    | ScopeVal(NullScope,_) -> "null scope"
    | ScopeVal(InnerScope(vals,_),_) -> (
        let content = List.map (fun (_,v) -> value_string v) (List.rev vals) in
        "[" ^ (String.concat ", " content) ^ "]"
    )

and expression_string expr = match expr with
    | Constant i -> string_of_int i
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
