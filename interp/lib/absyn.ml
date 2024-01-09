
type expression =
    | Constant of int
    | Route of route
    | Binop of string * expression * expression
    | Scope of stmt list
    | Func of string * expression
    | If of expression * expression * expression
    | Call of expression * expression

and route = step list

and step = 
    | Label of string
    | Index of expression
    | OutOf 

and stmt =
    | Named of string * expression
    | Anon of expression

and scope = 
    | NullScope
    | InnerScope of (string option * value) list * scope

and value =
    | Value of int * string option
    | Closure of string * expression * scope * string option
    | ScopeVal of scope * string option

and file = 
    | File of stmt