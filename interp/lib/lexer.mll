{
  open Parser
  open Exceptions
  let keyword_table = Hashtbl.create 53
  let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                      [
                        "if", IF;
                        "then", THEN;
                        "else", ELSE;
                        "match", MATCH;
                        "with", WITH;
                      ]

  let incr_linenum lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

rule lex = parse
        [' ' '\t']               { lex lexbuf }
    |   ('\r''\n' | '\n')        { incr_linenum lexbuf ; lex lexbuf }
    |   "//" [^ '\n' '\r']* ('\r''\n' | '\n' | eof)       { incr_linenum lexbuf ; lex lexbuf }
    |   '-'?['0'-'9']+ as lxm { CSTINT (int_of_string lxm) }
    |   ['A'-'Z' 'a'-'z' '''] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                { try
                    Hashtbl.find keyword_table id
                  with Not_found -> NAME id }
    |   '+'           { PLUS }
    |   '-'           { MINUS }
    |   '*'           { TIMES }
    |   '='           { EQ }
    |   "!="          { NEQ }
    |   '<'           { LT }
    |   '>'           { GT }
    |   "<="          { LTEQ }
    |   ">="          { GTEQ }
    |   '('           { LPAR }
    |   ')'           { RPAR }
    |   '['           { LBRAKE }
    |   ']'           { RBRAKE }
    |   ','           { COMMA }
    |   '^'           { UP }
    |   '.'           { DOT }
    |   '|'           { PIPE }
    |   '\\'          { LAMBDA }
    |   ':'           { COLON }
    |   "->"          { ARROW }
    |   '_'           { UNDERSCORE }
    |   _             { raise (Failure(Some((Lexing.lexeme_start_p lexbuf).pos_fname), Some((Lexing.lexeme_start_p lexbuf).pos_lnum), ("Unknown token"))) }
    |   eof           { EOF }

and start filename = parse
       "" { lex lexbuf }
