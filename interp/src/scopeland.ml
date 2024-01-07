open Str
open Scopelandlib.Exceptions
open Scopelandlib.Absyn

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

let rec route_string label =
  match label with
  | Label(ln) -> ln
  | Index(_) -> "[_]"
  | InTo(ln,rt) -> ln ^ "." ^ (route_string rt)
  | OutOf rt -> "^."^ (route_string rt)

let rec expression_string expr = match expr with
  | Constant i -> string_of_int i
  | Route rt -> route_string rt
  | Binop(op, expr1, expr2) -> "(" ^ expression_string expr1 ^ " " ^ op ^ " " ^ expression_string expr2 ^ ")"
  | Scope(stmts,_) -> (
    let content = List.map (fun stmt -> match stmt with | Named(n,_) -> n | Anon _ -> "?") stmts in
    "[" ^ (String.concat ", " content) ^ "]"
  ) 
  | Func(arg, expr, _) -> arg ^ " -> " ^ expression_string expr
  | If(cond,expr1,expr2) -> "if " ^ expression_string cond ^ " then " ^ expression_string expr1 ^ " else " ^ expression_string expr2
  | Call(func, expr) -> expression_string func ^ " " ^ expression_string expr

let print_scope scope =
  let rec aux stmts lvl = match stmts with
    | [] -> ()
    | Named(n,Scope(stmts,_))::t -> Printf.printf "%s%s\n" (String.init lvl (fun _ -> '\t')) n ; aux stmts (lvl+1) ; aux t lvl
    | Named(n,_)::t -> Printf.printf "%s%s\n" (String.init lvl (fun _ -> '\t')) n ; aux t lvl
    | Anon(Scope(stmts,_))::t -> Printf.printf "%s?\n" (String.init lvl (fun _ -> '\t')) ; aux stmts (lvl+1) ; aux t lvl
    | Anon(_)::t -> Printf.printf "%s?\n" (String.init lvl (fun _ -> '\t')) ; aux t lvl
  in
  let rec print scp lvl = 
    match scp with
    | NullScope -> Printf.printf "%s?\n" (String.init lvl (fun _ -> '\t')) ; Printf.printf "_\n"
    | OuterScope stmts -> Printf.printf "%s?\n" (String.init lvl (fun _ -> '\t')) ; Printf.printf "outer\n" ; aux stmts (lvl+1)
    | InnerScope (stmts, scp) -> Printf.printf "%s?\n" (String.init lvl (fun _ -> '\t')) ; Printf.printf "inner\n" ; aux stmts (lvl+1) ; print scp (lvl+1)
  in
  print scope 0

let get_stmts_of_scope scope = match scope with
  | NullScope -> raise_failure "Null scope"
  | OuterScope stmts 
  | InnerScope(stmts,_) -> stmts

let add_to_local_scope a scope = match scope with
  | NullScope -> raise_failure "Null scope"
  | OuterScope stmts -> OuterScope(a::stmts)
  | InnerScope(stmts,scp) -> InnerScope(a::stmts,scp)

let rec route_lookup route scope =
  print_scope scope ;
  let scope_stmts = get_stmts_of_scope scope in
  match route with
  | Label(ln) -> ( Printf.printf "looking for label %s\n" ln; print_scope scope;
    List.find_map (fun stmt -> match stmt with
    | Named(n,e) -> if n = ln then Some((e,scope)) else None
    | _ -> None
    ) scope_stmts
  )
  | Index(e) -> ( Printf.printf "looking for index\n"; print_scope scope;
    match interpret_expression e scope with
    | (Constant v,_) -> (
      match List.nth_opt (List.rev scope_stmts) v with
      | Some(Named(_,e)) | Some(Anon(e)) -> Some(e, scope)
      | None -> None
    )
    | _ -> raise_failure "Indexing with non-constant value"
  )
  | InTo(ln,rt) -> ( Printf.printf "looking for into %s\n" ln; print_scope scope;
    List.find_map (fun stmt -> match stmt with
      | Named(n,Scope(stmts,scp)) -> (match scp with 
        | NullScope -> if n = ln then route_lookup rt (InnerScope(stmts, scope)) else None
        | OuterScope _ -> if n = ln then route_lookup rt scp else None
        | InnerScope _ -> if n = ln then route_lookup rt scp else None
      )
      | _ -> None
    ) scope_stmts
  )
  | OutOf rt -> (Printf.printf "looking for out\n"; print_scope scope; match scope with 
    | NullScope -> raise_failure "Null scope"
    | OuterScope _ -> raise_failure "Cannot escape the outermost scope"
    | InnerScope (_,scp) -> route_lookup rt scp
  )

and interpret_expression expr scope : (expression * scope) = match expr with
  | Constant _ -> (expr, scope)
  | Route(rt) -> (match route_lookup rt scope with
    | Some(e,_) -> interpret_expression e scope
    | None -> raise_failure ("Unknown label: " ^ route_string rt)
  )
  | Binop (op,expr1,expr2) -> (
    let (expr1,_) = interpret_expression expr1 scope in
    let (expr2,_) = interpret_expression expr2 scope in
    match expr1, expr2, op with 
    | Constant(x),Constant(y),"+" -> (Constant(x + y), scope)
    | Constant(x),Constant(y),"-" -> (Constant(x - y), scope)
    | Constant(x),Constant(y),"*" -> (Constant(x * y), scope)
    | Constant(x),Constant(y),"<" -> if x < y then (Constant 1, scope) else (Constant 0, scope)
    | _ -> raise_failure "Unknown binop"
  )
  | Scope(exprs,scp) -> (match scp with
    | NullScope -> (
      let (res, def_scp) = interpret_scope exprs (InnerScope([], scope)) in
      (res, add_to_local_scope (Anon(Scope(exprs, def_scp))) scope) (*Not correct*)
    )
    | OuterScope _
    | InnerScope _ -> interpret_scope exprs scp
  ) 
  | Func _ -> (expr, scope)
  | Call(func,arg) -> ( Printf.printf "Calling %s\n" (expression_string func) ; print_scope scope ; 
    match interpret_expression func scope with
    (*| (Route rt,_) -> (
      match route_lookup rt scope with
      | None -> raise_failure ("Unknown label: " ^ (route_string rt))
      | Some((e,def_scp)) -> (
        match e with
        | Func(arg_n,body,_) -> ( 
          interpret_expression body (add_to_local_scope (Named(arg_n, interpret_expression arg scope |> fst)) def_scp)
        )
        | Scope(Named(_,Func(arg_n,body,_))::_, def_scp)
        | Scope(Anon(Func(arg_n,body,_))::_, def_scp) -> (
          interpret_expression body (add_to_local_scope (Named(arg_n, interpret_expression arg scope |> fst)) def_scp)
        )
        | _ -> raise_failure ("Call to non-callable: " ^ route_string rt)
      )
    )*)
    | (Func(arg_n,body,def_scp),_) -> ( 
      let (arg_expr,_) = interpret_expression arg scope in
      let (result,scp) = interpret_expression body (add_to_local_scope (Named(arg_n, arg_expr)) def_scp)
      in match result with
      | Func(arg,body,_) -> (Func(arg,body,add_to_local_scope (Named(arg_n, arg_expr)) def_scp),scp)
      | _ -> (result,scp)
    )
    | _ -> raise_failure ("Call to non-callable: " ^ expression_string func)
  )
  | If(cond,expr1,expr2) -> (match interpret_expression cond scope |> fst with
    | Constant 0 -> interpret_expression expr2 scope
    | _ -> interpret_expression expr1 scope
  )

and interpret_scope stmts scope : (expression * scope) = 
  match stmts with
  | [] -> (Constant 0, scope)
  | h::t -> ( match h with
    | Named(n,Func(arg,body,def_scp)) -> (
      let (_,rest_scope) = interpret_scope t scope in
      match def_scp with 
      | NullScope -> (Func(arg,body,rest_scope), add_to_local_scope (Named(n,Func(arg,body,rest_scope))) rest_scope)
      | OuterScope _ -> (Func(arg,body,def_scp), add_to_local_scope h rest_scope)
      | InnerScope _ -> (Func(arg,body,def_scp), add_to_local_scope h rest_scope)
    )
    | Anon(Func(arg,body,def_scp)) -> (
      let (_,rest_scope) = interpret_scope t scope in
      match def_scp with 
      | NullScope -> (Func(arg,body,rest_scope), add_to_local_scope (Anon(Func(arg,body,rest_scope))) rest_scope)
      | OuterScope _ -> (Func(arg,body,def_scp), add_to_local_scope h rest_scope)
      | InnerScope _ -> (Func(arg,body,def_scp), add_to_local_scope h rest_scope)
    )
    | Named(n,Scope(stmts,def_scp)) -> (
      let (_,rest_scope) = interpret_scope t scope in
      let (_,inner_scope) = match def_scp with
        | NullScope -> interpret_scope stmts (InnerScope([], rest_scope))
        | OuterScope stmts -> interpret_scope stmts def_scp
        | InnerScope (stmts,_) -> interpret_scope stmts def_scp
      in
      (Constant 1, add_to_local_scope (Named(n,Scope(stmts, inner_scope))) rest_scope)
    )
    | Anon(Scope(stmts,def_scp)) -> (
      let (_,rest_scope) = interpret_scope t scope in
      let (_,inner_scope) = match def_scp with
        | NullScope -> interpret_scope stmts (InnerScope([], scope))
        | OuterScope stmts -> interpret_scope stmts def_scp
        | InnerScope (stmts,_) -> interpret_scope stmts def_scp
      in
      (Constant 1, add_to_local_scope (Anon(Scope(stmts,inner_scope))) rest_scope)
    )
    | _ -> (
      let (_,rest_scope) = interpret_scope t scope in
      interpret_statement h (add_to_local_scope h rest_scope)
    )
  )

and interpret_statement stmt scope : (expression * scope) = match stmt with
  | Named(_,expr) -> interpret_expression expr scope
  | Anon expr -> interpret_expression expr scope

and interpret file = match file with File(stmt) -> interpret_statement stmt (OuterScope [])

let () =
  let input = resolve_input () in
  let absyn = Scopelandlib.Parser.main (Scopelandlib.Lexer.start input) (Lexing.from_string (read_file input)) in
  try 
    let result = interpret absyn |> fst in
    Printf.printf "%s\n" (expression_string result)
  with | Failure(_,_,exp) -> Printf.printf "Failure: %s\n" exp