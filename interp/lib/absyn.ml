
type expression =
    | Constant of int
    | Route of route
    | Binop of string * expression * expression
    | Scope of stmt list * scope
    | Func of string * expression 
    | If of expression * expression * expression
    | Call of route * expression

and route = 
    | Label of string
    | Index of expression
    | InTo of string * route
    | OutOf of route

and stmt =
    | Named of string * expression
    | Anon of expression

and scope = 
    | NullScope
    | OuterScope of stmt list
    | InnerScope of stmt list * scope

and file = 
    | File of stmt