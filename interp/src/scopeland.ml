open Scopelandlib.Exceptions
open Scopelandlib.Absyn

let () = Printexc.record_backtrace true

let resolve_input () =
  try (
    if Array.length Sys.argv < 1 then raise_failure "No file given to compile"
    else let input = Sys.argv.(1) in
    if not(String.ends_with ~suffix:".scl" input) then raise_failure "Invalid input file extension"
    else if not (Sys.file_exists input) then (Printf.printf "%s\n" input; raise_failure "Input file does not exist")
    else input
  ) with
  | Invalid_argument _ -> raise_failure "No file given to compile"
  | ex -> raise ex

let route_to_path start route =
  let rec aux rt acc = match rt with
    | [] -> (String.concat "/" (List.rev acc))^".scl"
    | Label n :: t -> aux t (n::acc)
    | OutOf :: t -> aux t (".."::acc)
    | FullOut :: t -> aux t ["/"]
    | Index _ :: _ -> raise_failure "Indexing for imports is not supported"
  in
  aux route [start]

let read_file path =
  let file = open_in path in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  content

let read_input input = 
  Scopelandlib.Parser.main (Scopelandlib.Lexer.start input) (Lexing.from_string (read_file input))

let get_values_of_scope scope = match scope with
  | NullScope _ -> raise_failure "IntegerVal lookup in the Null scope"
  | InnerScope(vals,_,_,_) -> vals

let global_scope scp = match scp with
  | NullScope dir -> NullScope dir
  | InnerScope(_,_,NullScope _,_) -> scp
  | InnerScope(_,_,g_scp,_) -> g_scp

let add_to_local_scope adds scope = match scope with
  | NullScope _ -> raise_failure "Scope lookup in the Null scope"
  | InnerScope(vals,scp,g_scp,dir) -> InnerScope(adds @ vals,scp,g_scp,dir)

let rec route_lookup route scope =
  let rec aux route result = 
    match route with
    | [] -> result
    | h :: t -> ( match result with
      | ScopeVal(scp,_) -> ( match h with
        | Label l -> (
          let scope_vals = get_values_of_scope scp in
          let lookup = List.find_opt (fun v -> match v with
            | (Some n, _) -> n = l
            | _ -> false
          ) scope_vals in
          match lookup with
          | Some(_,v) -> aux t v
          | None -> raise_failure ("No such name in scope: "^l)
        )
        | Index e -> (
          match interpret_expression None e scope with
          | (IntegerVal(i,_),_) -> (
            let scope_vals = get_values_of_scope scp in
            let (i,scope_vals) = if i >= 0 then (i, List.rev scope_vals) else ((abs i)-1, scope_vals) in
            match List.nth_opt scope_vals (abs i) with
            | Some(_,v) -> aux t v
            | None -> raise_failure "Index out of bounds"
          )
          | (v,_) -> raise_failure ("Indexing with non-integer value: " ^ value_string v)
        )
        | OutOf -> (
          match scp with
          | InnerScope(_,NullScope _,_,_)
          | NullScope _ -> raise_failure "OutOf: Attempt to escape the Null scope"
          | InnerScope(_,scp,_,_) -> aux t (ScopeVal(scp,None))
        )
        | FullOut -> aux t (ScopeVal(global_scope scp,None))
      )
      | _ -> raise_failure "Routing into a non-scope value"
    )
  in
  aux route (ScopeVal(scope,None))

and interpret_expression stmt_name_opt expr scope : (value * scope) = 
  match expr with
  | Integer i -> (IntegerVal(i,stmt_name_opt), scope)
  | String s -> (StringVal(s,stmt_name_opt), scope)
  | Route(rt) -> (route_lookup rt scope, scope)
  | Binop ("=",Scope [], expr)
  | Binop ("=",expr, Scope []) -> ( match interpret_expression None expr scope with
    | (ScopeVal(InnerScope([],_,_,_),_),_) -> (IntegerVal(1, stmt_name_opt), scope)
    | _ -> (IntegerVal(0, stmt_name_opt), scope)
  )
  | Binop ("!=",Scope [], expr)
  | Binop ("!=",expr, Scope []) -> ( match interpret_expression None expr scope with
    | (ScopeVal(InnerScope([],_,_,_),_),_) -> (IntegerVal(0, stmt_name_opt), scope)
    | _ -> (IntegerVal(1, stmt_name_opt), scope)
  )
  | Binop (op,expr1,expr2) -> (
    let (val1,_) = interpret_expression None expr1 scope in
    let (val2,_) = interpret_expression None expr2 scope in
    match val1, val2, op with 
    | IntegerVal(x,_),IntegerVal(y,_),"+" -> (IntegerVal(x + y, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),"-" -> (IntegerVal(x - y, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),"*" -> (IntegerVal(x * y, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),"=" -> if x = y then (IntegerVal(1, stmt_name_opt), scope) else (IntegerVal(0, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),"!=" -> if not(x = y) then (IntegerVal(1, stmt_name_opt), scope) else (IntegerVal(0, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),"<" -> if x < y then (IntegerVal(1, stmt_name_opt), scope) else (IntegerVal(0, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),">" -> if x > y then (IntegerVal(1, stmt_name_opt), scope) else (IntegerVal(0, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),"<=" -> if x <= y then (IntegerVal(1, stmt_name_opt), scope) else (IntegerVal(0, stmt_name_opt), scope)
    | IntegerVal(x,_),IntegerVal(y,_),">=" -> if x >= y then (IntegerVal(1, stmt_name_opt), scope) else (IntegerVal(0, stmt_name_opt), scope)
    | StringVal(x,_),StringVal(y,_),"+" -> (StringVal(x^y,None), scope)
    | StringVal(s,_),a,"+" -> (StringVal(s^(a |> remove_name |> value_string),None), scope)
    | a,StringVal(s,_),"+" -> (StringVal((a |> remove_name |> value_string)^s,None), scope)
    | ScopeVal(scope,_), _, "&" -> (ScopeVal(add_to_local_scope [value_name val2, val2] scope,None), scope)
    | _ -> raise_failure ("Unknown binary operation: (" ^ value_string val1 ^" "^ op ^" "^ value_string val2 ^ ")")
  )
  | Scope exprs -> 
    let scp = interpret_scope exprs (InnerScope([], scope, global_scope scope, scope_dir scope)) in
    (ScopeVal(scp, stmt_name_opt), scope)
  | Func(args,body) -> (Closure(args, body, [], scope, stmt_name_opt), scope)
  | Call(func,arg) -> ( 
    match interpret_expression stmt_name_opt func scope with
    | (Closure([arg_n],body,bindings,def_scp,fun_n),_) -> ( 
      let (arg_val,_) = interpret_expression stmt_name_opt arg scope in
      let bindings = (arg_n,arg_val)::bindings in
      let func_c = Closure(List.map fst bindings |> List.rev,body,[],def_scp,fun_n) in
      match interpret_scope body (InnerScope(List.map (fun (n,v) -> (Some n, v)) bindings, add_to_local_scope [(fun_n, func_c)] def_scp, global_scope def_scp, scope_dir def_scp)) with
      | InnerScope((_,result)::_,_,_,_) -> (result, scope)
      | _ -> raise_failure "No result from function call"
    )
    | (Closure(arg_n::rest,body,bindings,def_scp,fun_n),_) -> ( 
      let (arg_val,_) = interpret_expression stmt_name_opt arg scope in
      (Closure(rest,body,(arg_n, arg_val)::bindings,def_scp,fun_n), scope)
    )
    | (v,_) -> raise_failure ("Call to non-callable: " ^ expression_string func ^ " -> " ^ value_string v)
  )
  | If(cond,expr1,expr2) -> (match interpret_expression stmt_name_opt cond scope with
    | (IntegerVal(0,_),_) -> interpret_expression stmt_name_opt expr2 scope
    | _ -> interpret_expression stmt_name_opt expr1 scope
  )
  | Match(expr, alts) -> (
    let (value, _) = interpret_expression stmt_name_opt expr scope in
    let rec do_match v (pat,res) = match v, pat with
      | IntegerVal(i,_), IntegerPat(ci) -> if i = ci then Some([None,v],res) else None
      | StringVal(s,_), StringPat(sp) -> if s = sp then Some([None,v],res) else None
      | IntegerVal _, IntegerTypePat -> Some([None,v],res)
      | StringVal _, StringTypePat -> Some([None,v],res)
      | ScopeVal(InnerScope([],_,_,_),_), Empty -> Some([None,v],res)
      | ScopeVal(InnerScope((_,h)::t,a,b,dir),c), ScopeList(tp,hp) -> ( match do_match h (hp,res), do_match (ScopeVal(InnerScope(t,a,b,dir),c)) (tp,res) with
        | Some(h_binds,_),Some(t_binds,_) -> Some(t_binds@h_binds,res)
        | _ -> None
      )
      | ScopeVal(InnerScope(vals,_,_,_),_), ScopeTuple(pats) -> (
        if List.length vals != List.length pats then None
        else (
          let matches = List.map2 (fun (_,v) p -> do_match v (p,res)) vals pats in
          if List.mem None matches then None
          else let bindings = List.map Option.get matches |> List.map fst |> List.flatten in Some(bindings, res)
          (*if List.for_all Option.is_some meme then Some(_,res)
          else None*)
        )
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
      let path = route_to_path (scope_dir scope) rt in 
      try (
        match read_input path with
        | File stmt -> let (v,_) = interpret_statement stmt (NullScope path) in Some(value_name v, v) 
      )
      with _ -> raise_failure ("Could not import: '" ^ route_string rt ^ "' aka. " ^ path)
    )
    | Named(n,Func(args,body)) -> ( 
      Some(Some n, Closure(args,body,[],rest_scope,Some n))
    )
    | Anon(Func(args,body)) -> ( 
      Some(None, Closure(args,body,[],rest_scope, None))
    )
    | Named(n,Scope(stmts)) -> ( 
      let inner_scope = interpret_scope stmts (InnerScope([], rest_scope, (if global_scope rest_scope |> is_null_scope then rest_scope else global_scope rest_scope), scope_dir scope)) in
      Some(Some n,ScopeVal(inner_scope,Some n))
    )
    | Anon(Scope(stmts)) -> ( 
      let inner_scope = interpret_scope stmts (InnerScope([], rest_scope, (if global_scope rest_scope |> is_null_scope then rest_scope else global_scope rest_scope), scope_dir scope)) in
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
  | Out _ -> raise_failure "Printing outside scope"
  | Import rt -> (
    let path = route_to_path (scope_dir scope) rt in 
    try (
      match read_input path with
      | File stmt -> interpret_statement stmt (NullScope path)
    )
    with _ -> raise_failure ("Could not import: '" ^ route_string rt ^ "' aka. " ^ path)
  )

and interpret (File stmt) path = interpret_statement stmt (NullScope path)

let () =
  try 
    let path = resolve_input () in
    let dir = Sys.getcwd () ^ "/" ^ match String.rindex_opt path '/' with 
      | None -> ""
      | Some i -> String.sub path 0 i
    in
    let absyn = read_input path in
    match interpret absyn dir with
    | (ScopeVal(InnerScope((_,v)::_,_,_,_),_),_) (* If program is a scope, only print the last value *)
    | (v,_) -> Printf.printf "%s\n" (value_string v)
  with 
  | Failure(_,_,exp) -> Printf.printf "Failure: %s\n" exp
  | _ -> Printf.printf "Unknown error (likely a parser error)\n"