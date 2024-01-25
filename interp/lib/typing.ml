open Absyn
open Exceptions


let get_values_of_type_scope scope = match scope with
  | NullTypeScope -> raise_failure "Type lookup in the Null scope"
  | InnerTypeScope(vals,_) -> vals

let add_to_local_type_scope adds scope = match scope with
  | NullTypeScope -> raise_failure "TypeScope lookup in the Null scope"
  | InnerTypeScope(vals,scp) -> InnerTypeScope(adds @ vals,scp)

let rec route_type_lookup route lscope =
  let scope_vals = get_values_of_type_scope lscope in
  match route with 
  | [] -> raise_failure "Empty route"
  | h::t -> (
    let lookup = (match h with
    | Label(ln)-> (
      List.find_map (fun v -> match v with
        | (Some n,v) -> if n = ln then Some(v) else None
        | _ -> None
      ) scope_vals
    )
    | Index(_) -> failwith "index typing not implemented"
    | OutOf -> ( match lscope with 
      | InnerTypeScope (_,NullTypeScope)
      | NullTypeScope -> raise_failure "OutOf: Attempt to escape the Null scope"
      | InnerTypeScope (_,tscp) -> Some(T_Scope([],None,tscp) |> independant) 
    )
    | FullOut -> (
      let rec aux scp = match scp with
        | NullTypeScope -> raise_failure "FullOut: Hit the Null scope"
        | InnerTypeScope(_, NullTypeScope) -> Some(T_Scope([],None,NullTypeScope) |> independant)
        | InnerTypeScope(_, scp) -> aux scp
      in
      aux lscope
    )
  )
  in if t = [] then lookup
  else match lookup with
  | Some(Independant T_Scope(_,_,tscp)) -> route_type_lookup t tscp
  | _ -> None 
)

let type_compatible typ1 typ2 = match typ1, typ2 with
  | Independant T_Int, Independant T_Int -> true
  | Independant T_Func(_,_), Independant T_Func(_,_) -> failwith "type_comp for funcs not yet supported"
  | Independant T_Scope(_,_,_), Independant T_Scope(_,_,_) -> false
  | _,_ -> false

let scope_type t_scope : typ = match t_scope with
  | NullTypeScope -> T_Scope([],None,NullTypeScope)
  | InnerTypeScope(content,parent) -> (
    let tuple_type = List.map snd content in
    let array_type = match content with
      | [] -> None
      | h::t -> 
        let head_type = snd h in
        if List.for_all (fun (_,ty) -> type_compatible ty head_type) t then Some head_type else None 
    in
    T_Scope(tuple_type,array_type,parent)
  ) 

let append_to_scope_type t t_scope = match t_scope with
  | T_Scope(tuple,array,p) -> (
    let array = ( match array with
      | Some ty -> if type_compatible ty t then Some t else None
      | None -> None
    ) 
    in T_Scope(t::tuple,array,p)
  )
  | _ -> failwith "Not a scope type"

let rec type_expr expr t_scope = match expr with
  | Constant _ -> T_Int
  | Route rt -> ( match route_type_lookup rt t_scope with
      | Some Independant t -> t
      | _ -> failwith "Route did not type"
  )
  | Binop(op,expr1,expr2) -> (
    let (t1,t2) = (type_expr expr1 t_scope, type_expr expr2 t_scope) in
    match op,t1,t2 with
    | "+", T_Int, T_Int -> T_Int
    | "-", T_Int, T_Int -> T_Int
    | "*", T_Int, T_Int -> T_Int
    | "=", T_Int, T_Int -> T_Int
    | "!=", T_Int, T_Int -> T_Int
    | "<", T_Int, T_Int -> T_Int
    | ">", T_Int, T_Int -> T_Int
    | "<=", T_Int, T_Int -> T_Int
    | ">=", T_Int, T_Int -> T_Int
    | "&", (T_Scope _ as tscp), t -> append_to_scope_type (t |> independant) tscp
    | _ -> failwith "Unknown binop"
  )
  | Scope stmts -> type_scope stmts (InnerTypeScope([],t_scope)) |> scope_type
  | Func _ -> T_Func(T_Unit, Independant T_Unit)
  | If(cond,expr1,_) -> (
    if type_expr cond t_scope = T_Int then type_expr expr1 t_scope 
    else failwith "If condition not an int"
  )
  | Call _ -> T_Unit
  | Match _ -> T_Unit

and type_stmt stmt type_scope = match stmt with
  | Named(_,e) -> type_expr e type_scope
  | Anon e -> type_expr e type_scope
  | Out _ -> T_Unit
  | Import _ -> failwith "Cannot type an import yet"

and type_scope stmts t_scope : typ_scope = 
  match stmts with
  | [] -> t_scope
  | h::t -> ( 
    let rest_scope = type_scope t t_scope in
    let addition = match h with
    | Out _ -> None
    | Import _ -> failwith "Cannot type an import yet"
    | Named(n,Func(_,_)) -> ( 
      Some(Some n, T_Func(T_Int, Independant T_Int))
    )
    | Anon(Func(_,_)) -> ( 
      Some(None, T_Func(T_Int, Independant T_Int))
    )
    | Named(n,Scope(stmts)) -> ( 
      let inner_scope = type_scope stmts (InnerTypeScope([], rest_scope)) in
      Some(Some n, inner_scope |> scope_type)
    )
    | Anon(Scope(stmts)) -> ( 
      let inner_scope = type_scope stmts (InnerTypeScope([], rest_scope)) in
      Some(None, inner_scope |> scope_type)
    )
    | Named(n,_) -> ( 
      Some(Some n, type_stmt h rest_scope)
    )
    | Anon _ -> ( 
      Some(None, type_stmt h rest_scope)
    )
    in
    match addition with
    | None -> rest_scope
    | Some(n,t) -> add_to_local_type_scope [n,t |> independant] rest_scope
  )