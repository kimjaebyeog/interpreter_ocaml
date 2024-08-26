(* open Env *)
open Comm
open Error

let filenames = ref []

let rec main = function
  [] -> ()
  | x :: xs ->
    try 
      let lexbuf = Lexing.from_channel x 
      in let rec loop env tenv= 
        try
          let comm = MyParser.main MyLexer.token lexbuf in
          let Command_result (env',tenv') = exe_comm env tenv comm in
          (* print_comm_result (Command_result (l,env',tenv'));  *) (* moved to comm.ml *)
          loop env' tenv'
        with 
        | MyParser.Error -> handle_error (Parse_error ("Unexpected Token " ^ Lexing.lexeme lexbuf ^ " at line " ^ (string_of_int(Lexing.lexeme_start_p lexbuf).pos_lnum ) ^ ", offset "^ (string_of_int((Lexing.lexeme_start_p lexbuf).pos_cnum - (Lexing.lexeme_start_p lexbuf).pos_bol)) )); loop env tenv
        | e -> handle_error e; loop env tenv
      in loop Env.emptyenv []
      (* todo : empty tenv *)

    with 
      | End_of_file -> (); close_in x; main xs
      
let _ =
  Arg.parse [] (fun s -> filenames := s :: !filenames) ""; (* Arg.parse [(key,spec,doc),...] anon_fun usage_msg  //key에 없는 인자는 anon_fun에 적용 *)
  if !filenames = []
  then main [stdin] 
  else let channels = List.map (fun s -> open_in s) (List.rev !filenames) 
    in main channels

(* todo : output to file(s) *)