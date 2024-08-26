exception Lex_error of string
exception Parse_error of string
exception Eval_error of string
exception Ty_error of string

let handle_error e = 
  match e with
  | Parse_error msg -> print_endline ("Parse_error : " ^ msg)
  | Eval_error msg -> print_endline ("Eval_error : " ^ msg)
  | Lex_error msg -> print_endline ("Lex_error : " ^ msg)
  | Ty_error msg -> print_endline ("Ty_error : " ^ msg)
  | End_of_file -> raise End_of_file (* throw to main.ml, proceed to next file *)
  | _ -> print_endline ("Unexpected Error"); raise e (* throw to ocaml *)

(* todo:
  take lexbuf and generate msg here
  enrich message with menhir's .message for parse error
 *)