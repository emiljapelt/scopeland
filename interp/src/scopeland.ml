open Str
open Scopelandlib.Exceptions
open Scopelandlib.Absyn
open Scopelandlib.Typing

let () = Printexc.record_backtrace true

let resolve_input () =
  try (
    let input = Sys.argv.(1) in
    if not (Sys.file_exists input) then (Printf.printf "%s\n" input; raise_failure "Input file does not exist")
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.scl$|}) input 0 then input
    else raise_failure "Invalid input file extension"
  ) with
  | Invalid_argument _ -> raise_failure "No file given to compile"
  | ex -> raise ex
  
let read_file path =
  let file = open_in path in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  content

let read_input input = 
  Scopelandlib.Parser.main (Scopelandlib.Lexer.start input) (Lexing.from_string (read_file input))

let rec route_lookup route scope lscope =
  let scope_vals = get_values_of_scope lscope in
  match route with
  | [] -> raise_failure "Empty route"
  | h::t -> ( 
    let lookup = ( match h with
      | Label(ln) -> (
        List.find_map (fun v -> match v with
          | (Some n,v) -> if n = ln then Some(v) else None
          | _ -> None
        ) scope_vals
      )
      | Index(e) -> ( 
        match interpret_expression None e scope with
        | (Value(i,_),_) -> ( 
          let (i,scope_vals) = if i >= 0 then (i, List.rev scope_vals) else ((abs i)-1, scope_vals) in
          match List.nth_opt scope_vals (abs i) with
          | Some(_,v) -> Some(v)
          | None -> None
        )
        | (v,_) -> raise_failure ("Indexing with non-constant value: " ^ value_string v)
      )
      | OutOf-> ( 
        match lscope with 
        | InnerScope (_,NullScope)
        | NullScope -> raise_failure "OutOf: Attempt to escape the Null scope"
        | InnerScope (_,scp) -> Some(ScopeVal(scp,None)) 
      )
      | FullOut -> (
        let rec aux scp = match scp with
          | NullScope -> raise_failure "FullOut: Hit the Null scope"
          | InnerScope(_, NullScope) -> Some(ScopeVal(scp,None))
          | InnerScope(_, scp) -> aux scp
        in
        aux lscope
      )
    )
    in
    if t = [] then lookup
    else match lookup with
    | Some(ScopeVal(scp,_)) -> route_lookup t scope scp
    | _ -> None 
  )

and interpret_expression stmt_name_opt expr scope : (value * scope) = 
  match expr with
  | Constant i -> (Value(i,stmt_name_opt), scope)
  | Route(rt) -> ( match route_lookup rt scope scope with
    | Some(v) -> (v, scope)
    | None -> raise_failure ("Unknown label: " ^ route_string rt)
  )
  | Binop ("=",Scope [], expr)
  | Binop ("=",expr, Scope []) -> ( match interpret_expression None expr scope with
    | (ScopeVal(InnerScope([],_),_),_) -> (Value(1, stmt_name_opt), scope)
    | _ -> (Value(0, stmt_name_opt), scope)
  )
  | Binop ("!=",Scope [], expr)
  | Binop ("!=",expr, Scope []) -> ( match interpret_expression None expr scope with
    | (ScopeVal(InnerScope([],_),_),_) -> (Value(0, stmt_name_opt), scope)
    | _ -> (Value(1, stmt_name_opt), scope)
  )
  | Binop (op,expr1,expr2) -> (
    let (val1,_) = interpret_expression None expr1 scope in
    let (val2,_) = interpret_expression None expr2 scope in
    match val1, val2, op with 
    | Value(x,_),Value(y,_),"+" -> (Value(x + y, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"-" -> (Value(x - y, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"*" -> (Value(x * y, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"=" -> if x = y then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"!=" -> if not(x = y) then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"<" -> if x < y then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),">" -> if x > y then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"<=" -> if x <= y then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),">=" -> if x >= y then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | ScopeVal(scope,_), _, "&" -> (ScopeVal(add_to_local_scope [value_name val2, val2] scope,None), scope)
    | _ -> raise_failure ("Unknown binary operation: (" ^ value_string val1 ^" "^ op ^" "^ value_string val2 ^ ")")
  )
  | Scope exprs -> 
    let scp = interpret_scope exprs (InnerScope([], scope)) in
    (ScopeVal(scp, stmt_name_opt), scope)
  | Func(args,_,body) -> (Closure(args, body, [], scope, stmt_name_opt), scope)
  | Call(func,arg) -> ( 
    match interpret_expression stmt_name_opt func scope with
    | (Closure([(arg_n,arg_t)],body,bindings,def_scp,fun_n),_) -> ( 
      let (arg_val,_) = interpret_expression stmt_name_opt arg scope in
      let bindings = (arg_n,arg_t,arg_val)::bindings in
      let func_c = Closure(List.rev_map (fun (a,b,_) -> (a,b)) bindings,body,[],def_scp,fun_n) in
      match interpret_scope body (InnerScope((Some arg_n,arg_val)::(List.map (fun (n,_,v) -> (Some n, v)) bindings), add_to_local_scope [(fun_n, func_c)] def_scp)) with
      | InnerScope((_,result)::_,_) -> (result, scope)
      | _ -> raise_failure "No result from function call"
    )
    | (Closure((arg_n,arg_t)::rest,body,bindings,def_scp,fun_n),_) -> ( 
      let (arg_val,_) = interpret_expression stmt_name_opt arg scope in
      (Closure(rest,body,(arg_n,arg_t,arg_val)::bindings,def_scp,fun_n), scope)
    )
    | (v,_) -> raise_failure ("Call to non-callable: " ^ expression_string func ^ " -> " ^ value_string v)
  )
  | If(cond,expr1,expr2) -> (match interpret_expression stmt_name_opt cond scope with
    | (Value(0,_),_) -> interpret_expression stmt_name_opt expr2 scope
    | _ -> interpret_expression stmt_name_opt expr1 scope
  )
  | Match(expr, alts) -> (
    let (value, _) = interpret_expression stmt_name_opt expr scope in
    let rec do_match v (pat,res) = match v, pat with
      | Value(i,_), Concrete(ci) -> if i = ci then Some([None,v],res) else None
      | ScopeVal(InnerScope([],_),_), Empty -> Some([None,v],res)
      | ScopeVal(InnerScope((_,h)::t,a),b), ScopePat(tp,hp) -> ( match do_match h (hp,res), do_match (ScopeVal(InnerScope(t,a),b)) (tp,res) with
        | Some(h_binds,_),Some(t_binds,_) -> Some(t_binds@h_binds,res)
        | _ -> None
      )
      | _, Name n -> Some([Some n,v],res)
      | _, Any -> Some([None,v],res)
      | _ -> None
    in
    match List.find_map (do_match value) alts with
    | Some(binds,res) -> interpret_expression stmt_name_opt res (add_to_local_scope (List.rev binds) scope)
    | None -> raise_failure "No matching alternative"
  )

and interpret_scope stmts scope : scope = 
  match stmts with
  | [] -> scope
  | h::t -> ( 
    let rest_scope = interpret_scope t scope in
    let addition = match h with
    | Out expr -> (
      let (v,_) = interpret_expression None expr rest_scope in
      Printf.printf "%s\n" (value_string v); 
      None
    )
    | Import rt -> (
      let path = route_to_path rt in 
      try (
        match read_input path with
        | File stmt -> let (v,_) = interpret_statement stmt NullScope in Some(value_name v, v) 
      )
      with _ -> raise_failure ("Could not import: '" ^ route_string rt ^ "' aka. " ^ path)
    )
    | Named(n,Func(args,_,body)) -> ( 
      Some(Some n, Closure(args,body,[],rest_scope,Some n))
    )
    | Anon(Func(args,_,body)) -> ( 
      Some(None, Closure(args,body,[],rest_scope, None))
    )
    | Named(n,Scope(stmts)) -> ( 
      let inner_scope = interpret_scope stmts (InnerScope([], rest_scope)) in
      Some(Some n,ScopeVal(inner_scope,Some n))
    )
    | Anon(Scope(stmts)) -> ( 
      let inner_scope = interpret_scope stmts (InnerScope([], rest_scope)) in
      Some(None, ScopeVal(inner_scope,None))
    )
    | Named(n,_) -> ( 
      Some(Some n, interpret_statement h rest_scope |> fst)
    )
    | Anon _ -> ( 
      Some(None, interpret_statement h rest_scope |> fst)
    )
    in
    match addition with
    | None -> rest_scope
    | Some(n,v) -> add_to_local_scope [n,v] rest_scope
  )

and interpret_statement stmt scope : (value * scope) = match stmt with
  | Named(name,expr) -> interpret_expression (Some name) expr scope
  | Anon expr -> interpret_expression None expr scope
  | Out expr -> (
    let (v,_) = interpret_expression None expr scope in
    Printf.printf "%s\n" (value_string v); 
    (Unit None, scope)
  )
  | Import rt -> (
    let path = route_to_path rt in 
      try (
        match read_input path with
        | File stmt -> interpret_statement stmt NullScope
      )
      with _ -> raise_failure ("Could not import: '" ^ route_string rt ^ "' aka. " ^ path)
  )

and interpret file = match file with File(stmt) -> Printf.printf "type: %s\n" (type_stmt stmt NullTypeScope |> type_string) ; interpret_statement stmt NullScope

let () =
  try 
    let absyn = read_input (resolve_input ()) in
    match interpret absyn with
    | (ScopeVal(InnerScope((_,v)::_,_),_),_) (* If program is a scope, only print the last value *)
    | (v,_) -> Printf.printf "%s\n" (value_string v)
  with 
  | Failure(_,_,exp) -> Printf.printf "Failure: %s\n" exp
  | e -> raise e (*Printf.printf "Unknown error (likely a parser error)\n"*)