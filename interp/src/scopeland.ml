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
    let (v, _) = interpret_expression e scope in
    match List.nth_opt (List.rev scope_stmts) v with
    | Some(Named(_,e)) | Some(Anon(e)) -> Some(e, scope)
    | None -> None
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

and interpret_expression expr (scope:scope) : (int * scope) = match expr with
  | Constant(i) -> (i, scope)
  | Route(rt) -> (match route_lookup rt scope with
    | Some(e,_) -> interpret_expression e scope
    | None -> raise_failure ("Unknown label: " ^ route_string rt)
  )
  | Binop (op,expr1,expr2) -> (match op with 
    | "+" -> ((interpret_expression expr1 scope |> fst) + (interpret_expression expr2 scope |> fst), scope)
    | "-" -> ((interpret_expression expr1 scope |> fst) - (interpret_expression expr2 scope |> fst), scope)
    | "*" -> ((interpret_expression expr1 scope |> fst) * (interpret_expression expr2 scope |> fst), scope)
    | "<" -> if (interpret_expression expr1 scope |> fst) < (interpret_expression expr2 scope |> fst) then (1, scope) else (0, scope)
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
  | Func(_,_) -> (1,scope)
  | Call(n,arg) -> ( Printf.printf "Calling %s\n" (route_string n) ; print_scope scope ; 
    match route_lookup n scope with
    | None -> raise_failure ("Unknown label: " ^ (route_string n))
    | Some((e,def_scp)) -> (
      match e with
      | Func(arg_n,body) -> ( 
        interpret_expression body (add_to_local_scope (Named(arg_n, Constant(interpret_expression arg scope |> fst))) def_scp)
        )
      | Scope(Named(_,Func(arg_n,body))::_, def_scp)
      | Scope(Anon(Func(arg_n,body))::_, def_scp) -> (
        interpret_expression body (add_to_local_scope (Named(arg_n, Constant(interpret_expression arg scope |> fst))) def_scp)
        )
      | _ -> raise_failure ("Call to non-callable: " ^ route_string n)
    )
  )
  | If(cond,expr1,expr2) -> (match interpret_expression cond scope |> fst with
    | 0 -> interpret_expression expr2 scope
    | _ -> interpret_expression expr1 scope
  )

and interpret_scope stmts (scope:scope) : (int * scope) = 
  match stmts with
  | [] -> (0, scope)
  | h::t -> ( match h with
    | Named(_,Func _)
    | Anon(Func _) -> (
      let (_,rest_scope) = interpret_scope t scope in
      (1, add_to_local_scope h rest_scope)
    )
    | Named(n,Scope(stmts,def_scp)) -> (
      let (_,rest_scope) = interpret_scope t scope in
      let (_,inner_scope) = match def_scp with
        | NullScope -> interpret_scope stmts (InnerScope([], rest_scope))
        | OuterScope stmts -> interpret_scope stmts def_scp
        | InnerScope (stmts,_) -> interpret_scope stmts def_scp
      in
      (1, add_to_local_scope (Named(n,Scope(stmts, inner_scope))) rest_scope)
    )
    | Anon(Scope(stmts,def_scp)) -> (
      let (_,rest_scope) = interpret_scope t scope in
      let (_,inner_scope) = match def_scp with
        | NullScope -> interpret_scope stmts (InnerScope([], scope))
        | OuterScope stmts -> interpret_scope stmts def_scp
        | InnerScope (stmts,_) -> interpret_scope stmts def_scp
      in
      (1, add_to_local_scope (Anon(Scope(stmts,inner_scope))) rest_scope)
    )
    | _ -> (
      let (_,rest_scope) = interpret_scope t scope in
      interpret_statement h (add_to_local_scope h rest_scope)
    )
  )

and interpret_statement stmt (scope:scope) : (int * scope) = match stmt with
  | Named(_,expr) -> interpret_expression expr scope
  | Anon expr -> interpret_expression expr scope

and interpret file = match file with File(stmt) -> interpret_statement stmt (OuterScope [])

let () =
  let input = resolve_input () in
  let absyn = Scopelandlib.Parser.main (Scopelandlib.Lexer.start input) (Lexing.from_string (read_file input)) in
  try 
    let result = interpret absyn |> fst in
    Printf.printf "%i\n" result
  with | Failure(_,_,exp) -> Printf.printf "Failure: %s\n" exp