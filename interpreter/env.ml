open Syntax

let emptyenv : env = []

(* find from top of the stack -> latest bind is vaild*)
let rec find : env -> name -> value option
    = fun env name -> match env with
    [] -> None
    | (name', value) :: env' -> if name = name' then Some value else find env' name 
let add : env -> (name * value) -> env
    = fun env x -> x :: env
let append : env -> env -> env
    = ( @ )