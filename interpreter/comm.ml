open Syntax
open Infer
open Env
open Eval

let exe_comm : env -> ty_env -> command -> command_result
    = fun env tenv c -> 
    let (res, tenv') = infer_cmd tenv c
    in
    match c with
    | CExp e -> Command_result ([None, snd (List.hd res),eval env e], env, tenv')
    | CLet (x, e) -> let v = eval env e
        in Command_result ([Some x, snd (List.hd res), v], add env (x, v), tenv')
    | CRLet (f, x, e) -> let v = VRFun (f, x, e, env)
        in Command_result ([Some f, snd (List.hd res), v], add env (f, v), tenv')
    | CMRLet l -> 
        let rec mut_rec i l' env' = match l' with
            [] -> env'
            | (f, _, _) :: rest -> mut_rec (i+1) rest (add env' (f, VMRFun (i,l,env)))
        in let make_result i (f, v) = (Some f, snd (List.nth res i), v)
        in let fs = mut_rec 1 l Env.emptyenv
        in Command_result (List.mapi make_result (List.rev fs), Env.append fs env, tenv')

(* todo: print.ml로 분리 *)
let print_name = print_string 

let rec print_value = function 
    | VInt i -> print_int i 
    | VBool true -> print_string "true"
    | VBool false -> print_string " false"
    | VFun _ -> print_string "<fun>"
    | VRFun _ -> print_string "<rfun>"
    | VMRFun _ -> print_string "<mrfun>"
    | VCons (v1, v2) ->
        (* print_value v1; print_string "::"; print_value v2 *)
        let rec print_list_ele l = match l with 
            VCons (v1, v2) ->
                print_string";"; print_value v1; print_list_ele v2
            | VNil -> ()
            | _ -> () (* should not be able to reach here, todo : raise error*)
        in print_string "["; print_value v1; 
            print_list_ele v2; print_string "]"
    | VTuple (v1, v2) -> 
        let rec print_tuple_ele t = match t with 
            VTuple (v1, v2) ->
                print_string","; print_value v1; print_tuple_ele v2
            | VNil -> ()
            | _ -> () (* should not be able to reach here, todo : raise error*)
        in print_string "("; print_value v1; 
            print_tuple_ele v2; print_string ")"
    | VNil -> print_string "[]"

let rec print_type = function
(* int 형 인수를 추가하는것으로 연산순서를 지정하면 괄호를 더 쉽게 넣을 수 있음. *)
    | TInt -> print_string "int"
    | TBool -> print_string "bool"
    | TFun (t1, t2) -> 
        (match t1 with
        TFun _ -> print_string "("; print_type t1; print_string")"
        | _ -> print_type t1; );
        print_string " -> "; 
        (match t2 with
        TList _ | TTuple _ -> print_string "("; print_type t2; print_string")"
        | _-> print_type t2)
    | TVar i -> print_int i; print_string "'"
    (* todo : arrange i *)
    | TList t -> 
        (match t with
        TFun _ | TTuple _ -> print_string "("; print_type t; print_string")"
        | _-> print_type t); print_string " list"
    | TTuple (t1, TNil) ->
        (match t1 with
        TFun _ -> print_string "("; print_type t1; print_string")"
        | _ -> print_type t1; )
    | TTuple (t1, t2) ->
        (match t1 with
        TFun _ | TTuple _ -> print_string "("; print_type t1; print_string")"
        | _ -> print_type t1; );
        print_string " * "; 
        (match t2 with
        TFun _ -> print_string "("; print_type t2; print_string")"
        | _ -> print_type t2; );
    | TNil -> () 
    
let rec print_comm_result : command_result -> unit
    = fun r -> match r with
    Command_result ((None, t, v)::rest, env, tenv) -> 
        print_string "- : " ;
        print_type t;
        print_string " = " ;
        print_value v;
        print_newline ();
        print_comm_result (Command_result (rest, env, tenv)) 
    | Command_result ((Some x, t, v)::rest, env, tenv) ->
        print_string "val ";
        print_name x;
        print_string " : " ;
        print_type t;
        print_string " = ";
        print_value v;
        print_newline ();
        print_comm_result (Command_result (rest, env, tenv))     
    | Command_result ([], _, _) -> ()