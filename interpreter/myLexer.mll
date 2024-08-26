{
  open MyParser
  open Error
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| '\n'        { Lexing.new_line lexbuf; token lexbuf }
| space+      { token lexbuf }
| "let"       { LET }
| "rec"       { REC }
| "and"       { AND }
| "in"        { IN  }
| "fun"       { FUN  }
| "->"        { RARROW }
| "if"        { IF }
| "then"      { THEN }
| "else"      { ELSE }
| "match"     { MATCH }
| "with"      { WITH }
| "end"       { END }
| "_"         { UNDERSCORE }
| '|'         { BAR }
| '='         { EQ }
| '<'         { LT }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { MUL }
| '/'         { DIV }
| ','         { COMMA }
| '('         { LPAR }
| ')'         { RPAR }
| '['         { LBRK }
| ']'         { RBRK }
| "::"        { DCOLON }
| ';'         { SEMI }
| ";;"        { DSEMI }
| "true"      { TRUE }
| "false"     { FALSE }
| digit+ as n { INT (int_of_string n) }
| ident  as n { ID n }
| eof         { EOF  }
| _           { raise (Lex_error ("Unknown Token " ^ Lexing.lexeme lexbuf ^ " at line " ^ (string_of_int(Lexing.lexeme_start_p lexbuf).pos_lnum ) ^ ", offset "^ (string_of_int((Lexing.lexeme_start_p lexbuf).pos_cnum - (Lexing.lexeme_start_p lexbuf).pos_bol)) ))}
