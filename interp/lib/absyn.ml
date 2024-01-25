open Exceptions

type typ = 
  | T_Unit
  | T_Int
  | T_Poly of char
  | T_Func of typ * typ
  | T_Scope of scope_tuple_type * scope_list_type * typ_scope

and typ_scope = 
  | NullTypeScope
  | InnerTypeScope of (string option * typ) list * typ_scope

and scope_tuple_type = typ list

and scope_list_type = typ option

and expression =
    | Constant of int
    | Route of route
    | Binop of string * expression * expression
    | Scope of stmt list
    | Func of (string * typ) list * stmt list
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
    | Unit of string option
    | Value of int * string option
    | Closure of (string * typ) list * (stmt list) * ((string * typ * value) list) * scope * string option
    | ScopeVal of scope * string option

and file = 
    | File of stmt

let value_name v = match v with
    | Unit n -> n
    | Value(_,n)
    | Closure(_,_,_,_,n)
    | ScopeVal(_,n) -> n

let route_to_path route =
    let rec aux rt acc = match rt with
        | [] -> (String.concat "/" (List.rev acc))^".scl"
        | Label n :: t -> aux t (n::acc)
        | OutOf :: t -> aux t (".."::acc)
        | FullOut :: t -> aux t ["/"]
        | Index _ :: _ -> raise_failure "Indexing for imports is not supported"
    in
    aux route ["."]
      

let get_values_of_scope scope = match scope with
    | NullScope -> raise_failure "Value lookup in the Null scope"
    | InnerScope(vals,_) -> vals

let add_to_local_scope adds scope = match scope with
    | NullScope -> raise_failure "Scope lookup in the Null scope"
    | InnerScope(vals,scp) -> InnerScope(adds @ vals,scp)

let rec type_string typ = match typ with
  | T_Unit -> "unit"
  | T_Poly c -> "'"^Char.escaped c
  | T_Int -> "int"
  | T_Func(f, r) -> "("^type_string f^" -> "^type_string r ^")"
  | T_Scope(tuple,list,_) -> (
    let tuple_string = List.map type_string tuple |> String.concat "," in
    let list_string = match list with
    | Some(t) -> "| "^type_string t^"[]"
    | None -> ""
    in
    "("^tuple_string^") "^list_string
  )

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
    | Unit(Some n) -> n ^ ": ()"
    | Unit None -> "()"
    | Value(i,Some n) -> n ^ ": " ^ string_of_int i
    | Value(i,None) -> string_of_int i
    | Closure(args_n,body,_,_,Some n) -> n ^ ": " ^ String.concat " " (List.map fst args_n) ^ " -> " ^ expression_string (Scope body)
    | Closure(args_n,body,_,_,None) -> String.concat " " (List.map fst args_n) ^ " -> " ^ expression_string (Scope body)
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
    | Func(args, stmts) -> (String.concat " " (List.map fst args)) ^ " -> " ^ expression_string (Scope stmts)
    | If(cond,expr1,expr2) -> "if " ^ expression_string cond ^ " then " ^ expression_string expr1 ^ " else " ^ expression_string expr2
    | Call(func, expr) -> expression_string func ^ " " ^ expression_string expr
    | Match(expr, _) -> "match " ^ expression_string expr ^ " with"


