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

let rec value_string value = match value with
  | Value(i,Some n) -> n ^ ": " ^ string_of_int i
  | Value(i,None) -> string_of_int i
  | Closure(arg_n,body,_,Some n) -> n ^ ": " ^ arg_n ^ " -> " ^ expression_string body
  | Closure(arg_n,body,_,None) -> arg_n ^ " -> " ^ expression_string body
  | ScopeVal(NullScope,_) -> "null scope"
  | ScopeVal(OuterScope vals,_) -> (
    let content = List.map (fun (n,_) -> match n with | Some n -> n | None -> "?") vals in
    "[" ^ (String.concat ", " content) ^ "]"
  )
  | ScopeVal(InnerScope(vals,_),_) -> (
    let content = List.map (fun (n,_) -> match n with | Some n -> n | None -> "?") vals in
    "[" ^ (String.concat ", " content) ^ "]"
  )

and expression_string expr = match expr with
  | Constant i -> string_of_int i
  | Route rt -> route_string rt
  | Binop(op, expr1, expr2) -> "(" ^ expression_string expr1 ^ " " ^ op ^ " " ^ expression_string expr2 ^ ")"
  | Scope(stmts) -> (
    let content = List.map (fun stmt -> match stmt with | Named(n,_) -> n | Anon _ -> "?") stmts in
    "[" ^ (String.concat ", " content) ^ "]"
  ) 
  | Func(arg, expr) -> arg ^ " -> " ^ expression_string expr
  | If(cond,expr1,expr2) -> "if " ^ expression_string cond ^ " then " ^ expression_string expr1 ^ " else " ^ expression_string expr2
  | Call(func, expr) -> expression_string func ^ " " ^ expression_string expr

(*let print_scope scope =
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
  print scope 0*)

let get_values_of_scope scope = match scope with
  | NullScope -> raise_failure "Null scope1"
  | OuterScope vals 
  | InnerScope(vals,_) -> vals

let add_to_local_scope adds scope = match scope with
  | NullScope -> raise_failure "Null scope2"
  | OuterScope vals -> OuterScope(adds @ vals)
  | InnerScope(vals,scp) -> InnerScope(adds @ vals,scp)

let rec route_lookup route scope =
  let scope_vals = get_values_of_scope scope in
  match route with
  | Label(ln) -> (
    List.find_map (fun v -> match v with
    | (Some n,v) -> if n = ln then Some(v) else None
    | _ -> None
    ) scope_vals
  )
  | Index(e) -> ( 
    match interpret_expression None e scope with
    | (Value(i,_),_) -> (
      match List.nth_opt (List.rev scope_vals) i with
      | Some(_,v) -> Some(v)
      | None -> None
    )
    | _ -> raise_failure "Indexing with non-constant value"
  )
  | InTo(ln,rt) -> ( 
    List.find_map (fun v -> match v with
      | (_,ScopeVal(scp,Some n)) -> ( match scp with 
        | NullScope -> raise_failure "Null scope entered" 
        | _ -> if n = ln then route_lookup rt scp else None
      )
      | _ -> None
    ) scope_vals
  )
  | OutOf rt -> ( match scope with 
    | NullScope -> raise_failure "Null scope3"
    | OuterScope _ -> raise_failure "Cannot escape the outermost scope"
    | InnerScope (_,scp) -> route_lookup rt scp
  )

and interpret_expression stmt_name_opt expr scope : (value * scope) = 
  match expr with
  | Constant i -> (Value(i,stmt_name_opt), scope)
  | Route(rt) -> ( match route_lookup rt scope with
    | Some(v) -> (v, scope)
    | None -> raise_failure ("Unknown label: " ^ route_string rt)
  )
  | Binop (op,expr1,expr2) -> (
    let (val1,_) = interpret_expression stmt_name_opt expr1 scope in
    let (val2,_) = interpret_expression stmt_name_opt expr2 scope in
    match val1, val2, op with 
    | Value(x,_),Value(y,_),"+" -> (Value(x + y, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"-" -> (Value(x - y, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"*" -> (Value(x * y, stmt_name_opt), scope)
    | Value(x,_),Value(y,_),"<" -> if x < y then (Value(1, stmt_name_opt), scope) else (Value(0, stmt_name_opt), scope)
    | _ -> raise_failure "Unknown binop"
  )
  | Scope exprs -> ( 
    let scope = match scope with 
    | NullScope -> InnerScope([], scope)
    | OuterScope _ 
    | InnerScope _ -> InnerScope([], scope)
    in
    interpret_scope exprs scope
  ) 
  | Func(arg,body) -> (Closure(arg, body, scope, stmt_name_opt), scope)
  | Call(func,arg) -> ( 
    match interpret_expression stmt_name_opt func scope with
    | (Closure(arg_n,body,def_scp,fun_n) as func_c,_) -> ( 
      let (arg_val,_) = interpret_expression stmt_name_opt arg scope in
      let (result,_) = interpret_expression stmt_name_opt body (add_to_local_scope [(Some arg_n, arg_val);(fun_n, func_c)] def_scp)
      in match result with
      | _ -> (result, scope)
    )
    | _ -> raise_failure ("Call to non-callable: " ^ expression_string func)
  )
  | If(cond,expr1,expr2) -> (match interpret_expression stmt_name_opt cond scope with
    | (Value(0,_),_) -> interpret_expression stmt_name_opt expr2 scope
    | _ -> interpret_expression stmt_name_opt expr1 scope
  )

and interpret_scope stmts scope : (value * scope) = 
  match stmts with
  | [] -> (Value(0,None), scope)
  | h::t -> ( match h with
    | Named(n,Func(arg,body)) -> ( 
      let (_,rest_scope) = interpret_scope t scope in
      let closure = Closure(arg,body,rest_scope,Some n) in
      (closure, add_to_local_scope [(Some n,closure)] rest_scope)
    )
    | Anon(Func(arg,body)) -> ( 
      let (_,rest_scope) = interpret_scope t scope in
      let closure = Closure(arg,body,rest_scope, None) in
      (closure, add_to_local_scope [(None,closure)] rest_scope)
    )
    | Named(n,Scope(stmts)) -> ( 
      let (_,rest_scope) = interpret_scope t scope in
      let (_,inner_scope) = interpret_scope stmts (InnerScope([], rest_scope)) in
      (Value(1,Some n), add_to_local_scope [(Some n,ScopeVal(inner_scope,Some n))] rest_scope)
    )
    | Anon(Scope(stmts)) -> ( 
      let (_,rest_scope) = interpret_scope t scope in
      let (_,inner_scope) = interpret_scope stmts (InnerScope([], scope)) in
      (Value(1, None), add_to_local_scope [(None, ScopeVal(inner_scope,None))] rest_scope)
    )
    | Named(n,_) -> ( 
      let (_,rest_scope) = interpret_scope t scope in
      let (res,_) = interpret_statement h rest_scope in
      (res, add_to_local_scope [Some n,res] rest_scope)
    )
    | Anon _ -> ( 
      let (_,rest_scope) = interpret_scope t scope in
      let (res,_) = interpret_statement h rest_scope in
      (res, add_to_local_scope [None,res] rest_scope)
    )
  )

and interpret_statement stmt scope : (value * scope) = match stmt with
  | Named(name,expr) -> interpret_expression (Some name) expr scope
  | Anon expr -> interpret_expression None expr scope

and interpret file = match file with File(stmt) -> interpret_statement stmt NullScope

let () =
  let input = resolve_input () in
  let absyn = Scopelandlib.Parser.main (Scopelandlib.Lexer.start input) (Lexing.from_string (read_file input)) in
  try 
    let result = interpret absyn |> fst in
    Printf.printf "%s\n" (value_string result)
  with | Failure(_,_,exp) -> Printf.printf "Failure: %s\n" exp