open Syntax
open Env
open Error

let rec eval : env -> expr -> value 
    = fun env e ->
    (* try  *)
    begin
        match e with
        EValue v -> v
        | EVar x -> (
            match find env x with
            None -> raise (Eval_error ("Unbound Value : " ^ x))
            | Some v -> v
        )
        | ENeg e ->(
            match eval env e with
            VInt v -> VInt (-v)
            | _ -> raise (Eval_error "Tried to Negate(-) with non-Int")
        )
        | EBin (OpAdd, e1, e2) -> (
            match  (eval env e1, eval env e2) with
            (VInt v1,VInt v2) -> VInt(v1+v2)
            | _ -> raise (Eval_error "Tried to Add(+) with non-Int")
        )
        | EBin (OpSub, e1, e2) -> (
            match  (eval env e1, eval env e2) with
            (VInt v1,VInt v2) -> VInt(v1-v2)
            | _ -> raise (Eval_error "Tried to Sub(-) with non-Int")
        )
        | EBin (OpMul, e1, e2) -> (
            match  (eval env e1, eval env e2) with
            (VInt v1,VInt v2) -> VInt(v1*v2)
            | _ -> raise (Eval_error "Tried to Mul(*) with non-Int")
        )
        | EBin (OpDiv, e1, e2) -> (
            match  (eval env e1, eval env e2) with
            (VInt v1,VInt v2) -> VInt(v1/v2)
            | _ -> raise (Eval_error "Tried to Div(/) with non-Int")
        )
        | EBin (OpEq, e1, e2) -> (
            match (eval env e1, eval env e2) with
            (VInt v1, VInt v2) -> VBool (v1 = v2)
            | (VBool v1, VBool v2) -> VBool (v1 = v2)
            | (VCons (v11, v12), VCons (v21, v22)) ->
                VBool( (eval env (EBin (OpEq, EValue v11, EValue v21)) = VBool true)
                && (eval env (EBin (OpEq, EValue v12, EValue v22)) = VBool true))
            | (VTuple (v11, v12), VTuple (v21, v22)) ->
                VBool( (eval env (EBin (OpEq, EValue v11, EValue v21)) = VBool true)
                && (eval env (EBin (OpEq, EValue v12, EValue v22)) = VBool true))
            | (VNil, VNil) -> VBool true
            | (VNil, VCons _) -> VBool false
            | (VCons _, VNil) -> VBool false
            | (VFun _, VFun _) | (VRFun _, VRFun _) | (VMRFun _, VMRFun _)
                -> raise (Eval_error "Can't compare(=) functions")
            | _ -> raise (Eval_error "Tried to Compare(=) with different types")
        )
        | EBin (OpLt, e1, e2) -> (
            match (eval env e1, eval env e2) with
            (VInt v1, VInt v2) -> VBool (v1 < v2)
            | (VBool v1, VBool v2) -> VBool (v1 < v2)
            (* !!! list < list 는 사전순 비교  *)
            | (VCons (v11, v12), VCons (v21, v22)) ->
                VBool( (eval env (EBin (OpLt, EValue v11, EValue v21)) = VBool true)
                ||( (eval env (EBin (OpEq, EValue v11, EValue v21)) = VBool true)
                    && (eval env (EBin (OpLt, EValue v12, EValue v22)) = VBool true)))
            | (VTuple (v11, v12), VTuple (v21, v22)) ->
                VBool( (eval env (EBin (OpLt, EValue v11, EValue v21)) = VBool true)
                ||( (eval env (EBin (OpEq, EValue v11, EValue v21)) = VBool true)
                && (eval env (EBin (OpLt, EValue v12, EValue v22)) = VBool true)))
            | (VNil, VNil) -> VBool false
            | (VNil, VCons _) -> VBool true
            | (VCons _, VNil) -> VBool false
            | (VFun _, VFun _) | (VRFun _, VRFun _) | (VMRFun _, VMRFun _)
                -> raise (Eval_error "Can't compare(<) functions")
            | _ -> raise (Eval_error "Tried to Compare(<) different types")
        )
        | EIf (e0, e1, e2) -> (
            match eval env e0 with
            VBool b -> if b then eval env e1 else eval env e2
            | _ -> raise (Eval_error "Condition of If is not Bool")
        )
        | ELet (x, e1, e2) -> eval (add env (x, eval env e1)) e2
        | ERLet (f, x, e1, e2) -> let env' = add env (f, VRFun (f, x, e1, env))
            in eval env' e2
        | EMRLet (l, e) -> 
            let rec mut_rec i l' env' = match l' with
                [] -> env'
                | (f, _, _) :: rest -> mut_rec (i+1) rest (add env' (f, VMRFun (i,l,env)))
            in eval (mut_rec 1 l env) e
        | EFun (x, e) -> VFun (x, e, env)
        | EApp (e1, e2) -> begin
            match eval env e1 with
            VFun (x, e, oenv) ->
                eval (add oenv (x, eval env e2)) e
                (* env of closure has to come before env of outside *)
            (* add f(rec) to env and eval inside *)
            | VRFun (f, x, e, oenv) ->
                let env' = add (add oenv (f, VRFun (f, x, e, oenv))) (x, eval env e2) 
                in eval env' e
            | VMRFun (i, l, oenv) ->
                let rec mut_rec i l' env' = match l' with
                    [] -> env'
                    | (f, _, _) :: rest -> mut_rec (i+1) rest (add env' (f, (VMRFun (i,l,env))))
                in let (_, x, e) = List.nth l (i-1)
                in let env' = add (mut_rec 1 l oenv) (x, eval env e2) 
                in eval env' e
                (* let f1 and... 는
                    let self idx = match idx with
                        1 -> e1 ...
                    로 하고 내부의 fi 는 self i 로 표현 하면
                    자신만의 재귀로 구현 됨 *)
            (* e1 이 함수가 아니라는것을 알 수 있도록 에러를 발생시키는게 좋을것 *)
            | _ -> raise (Eval_error "Tried to applicate with non-function ")
            end
            | EMatch (e, l) ->
                let v = eval env e
                in let rec find_match : env -> pattern -> value -> env option
                    = fun env p v -> match p with
                    PValue pv -> if eval env (EBin (OpEq, EValue pv, EValue v)) = VBool true 
                        then Some env else None
                        (* pv = v 로 비교하는게 더 쉽지 않았나? *)
                    | PVar px ->
                       Some (add env (px, v))
                        (* px는 환경에서 찾으면 안되고 반드시 할당해야함. *)
                    | PList (p1, p2) -> begin 
                        match v with
                        VCons (v1, v2) -> begin
                            match find_match env p1 v1 with
                            Some env' -> find_match env' p2 v2
                            | None -> None
                            end 
                        | _ -> None
                        end
                    | PTuple (p1, p2) -> begin 
                        match v with
                        VTuple (v1, v2) -> begin
                            match find_match env p1 v1 with
                            Some env' -> find_match env' p2 v2
                            | None -> None
                            end 
                        | _ -> None
                        end
                    | PWild -> Some env
                in let rec pattern_match : value -> (pattern * expr) list -> value
                    = fun v l -> match l with
                    [] -> raise (Eval_error "There is no Pattern")
                    (* todo : raise Match_failure *)
                    | (p, e2) :: l' ->
                        match find_match env p v with
                        None -> pattern_match v l'
                        | Some env' -> eval env' e2
                in pattern_match v l
            | EList (e1, e2) -> begin
                match eval env e2 with
                VCons (v1, v2) -> VCons (eval env e1, VCons (v1, v2))
                (* VCons대신 VCons *)
                | VNil -> VCons (eval env e1, VNil)
                | _ -> raise ( Eval_error "Tried to Cons to non-list")
            end
            | ETuple (e1, e2) -> begin
                match eval env e2 with
                VTuple (v1, v2) -> VTuple (eval env e1, VTuple (v1, v2))
                | VNil -> VTuple (eval env e1, VNil)
                | _ -> raise (Eval_error "Tried to Cons to non-tuple")
                end
    end
    (* with Match_failure _ -> failwith "Unexpected Runtime Error : Match Failed" *)