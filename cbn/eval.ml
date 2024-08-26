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
            | Some (Thunk (e', env')) -> eval env' e'
            | Some (RThunk (f, e', env')) -> eval (add env' (f, RThunk(f, e', env'))) e'
            | Some (MRThunk (i, l, env)) ->
                let rec mut_rec i l' env' = match l' with
                    [] -> env'
                    | (f, _) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
                in eval (mut_rec 1 l env) (snd (List.nth l (i - 1)))
            (* | Some v -> v  *) (* unreacherble *)
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
            | (VCons (t11, t12), VCons (t21, t22)) ->
                (* ECons is evaled to VCons(thunk, thunk) *)
                VBool( (eval env (EBin (OpEq, EValue (eval env (EThunk t11)), EValue (eval env (EThunk t21)))) = VBool true)
                && (eval env (EBin (OpEq, EValue (eval env (EThunk t12)), EValue (eval env (EThunk t22)))) = VBool true))
            | (VTuple (t11, t12), VTuple (t21, t22)) ->
                VBool( (eval env (EBin (OpEq, EValue (eval env (EThunk t11)), EValue (eval env (EThunk t21)))) = VBool true)
                && (eval env (EBin (OpEq, EValue (eval env (EThunk t12)), EValue (eval env (EThunk t22)))) = VBool true))
            | (VNil, VNil) -> VBool true
            | (VNil, VCons _) -> VBool false
            | (VCons _, VNil) -> VBool false
            | (VFun _, VFun _) 
            (* | (VRFun _, VRFun _) | (VMRFun _, VMRFun _) *)
                -> raise (Eval_error "Can't compare(=) functions")
            | _ -> raise (Eval_error "Tried to Compare(=) with different types")
        )
        | EBin (OpLt, e1, e2) -> (
            match (eval env e1, eval env e2) with
            (VInt v1, VInt v2) -> VBool (v1 < v2)
            | (VBool v1, VBool v2) -> VBool (v1 < v2)
            (* !!! list < list 는 사전순 비교  *)
            | (VCons (t11, t12), VCons (t21, t22)) ->
                VBool( (eval env (EBin (OpLt, EValue (eval env (EThunk t11)), EValue (eval env (EThunk t21)))) = VBool true)
                ||((eval env (EBin (OpEq, EValue (eval env (EThunk t11)), EValue (eval env (EThunk t21)))) = VBool true)
                    && (eval env (EBin (OpLt, EValue (eval env (EThunk t12)), EValue (eval env (EThunk t22)))) = VBool true)))
            | (VTuple (t11, t12), VTuple (t21, t22)) ->
                VBool( (eval env (EBin (OpLt, EValue (eval env (EThunk t11)), EValue (eval env (EThunk t21)))) = VBool true)
                ||((eval env (EBin (OpEq, EValue (eval env (EThunk t11)), EValue (eval env (EThunk t21)))) = VBool true)
                    && (eval env (EBin (OpLt, EValue (eval env (EThunk t12)), EValue (eval env (EThunk t22)))) = VBool true)))
            | (VNil, VNil) -> VBool false
            | (VNil, VCons _) -> VBool true
            | (VCons _, VNil) -> VBool false
            | (VFun _, VFun _) 
            (* | (VRFun _, VRFun _) | (VMRFun _, VMRFun _) *)
                -> raise (Eval_error "Can't compare(<) functions")
            | _ -> raise (Eval_error "Tried to Compare(<) different types")
        )
        | EIf (e0, e1, e2) -> (
            match eval env e0 with
            VBool b -> if b then eval env e1 else eval env e2
            | _ -> raise (Eval_error "Condition of If is not Bool")
        )
        | ELet (x, e1, e2) -> eval (add env (x, Thunk (e1,env))) e2
        | ERLet (f, e1, e2) -> let env' = add env (f, RThunk (f, e1, env))
            in eval env' e2
        | EMRLet (l, e) -> 
            let rec mut_rec i l' env' = match l' with
                [] -> env'
                | (f, _) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
            in eval (mut_rec 1 l env) e
        | EFun (x, e) -> VFun (x, e, env)
        | EApp (e1, e2) -> begin
            match eval env e1 with
            VFun (x, e, oenv) ->
                eval (add oenv (x, Thunk (e2, env))) e
                (* env of closure has to come before env of outside *)
            | _ -> raise (Eval_error "Tried to applicate with non-function ")
            end
        | EMatch (e, l) ->
            let rec find_match : env -> pattern -> expr -> env option
            (* proceed eval to the level where we can check the matchenss with the pattern and not further*)
            (* eval of e is proceeded on (original)env, use env' for generated env after pattern match *)
                = fun env' p e -> match p with
                PValue pv -> if eval env (EBin (OpEq, EValue pv, e)) = VBool true 
                    then Some env' else None
                | PVar px ->
                    Some (add env' (px, Thunk(e,env)))
                    (* don't find px from env. *)
                | PList (p1, p2) -> begin 
                    match eval env e with
                    VCons (t1, t2) -> begin
                        match find_match env' p1 (EThunk t1) with
                        Some env'' -> find_match env'' p2 (EThunk t2)
                        | None -> None
                        end 
                    | _ -> None
                    end
                | PTuple (p1, p2) -> begin 
                    match eval env e with
                    VTuple (t1, t2) -> begin
                        match find_match env' p1 (EThunk t1) with
                        Some env'' -> find_match env'' p2 (EThunk t2)
                        | None -> None
                        end 
                    | _ -> None
                    end
                | PWild -> Some env'
            in let rec pattern_match : expr -> (pattern * expr) list -> value
                = fun v l -> match l with
                [] -> raise (Eval_error "There is no Pattern")
                (* todo : raise Match_failure *)
                | (p, e2) :: l' ->
                    match find_match env p v with
                    None -> pattern_match v l'
                    | Some env' -> eval env' e2
            in pattern_match e l

        | EList (e1, e2) -> 
            VCons (Thunk (e1,env),Thunk (e2,env))
        | ETuple (e1, e2) -> 
            VTuple (Thunk (e1,env),Thunk (e2,env))
        | EThunk (Thunk (e', env')) -> eval env' e'
        | EThunk (RThunk (f, e', env')) -> eval (add env' (f, RThunk(f, e', env'))) e'
        | EThunk (MRThunk (i, l, env)) ->
                let rec mut_rec i l' env' = match l' with
                    [] -> env'
                    | (f, _) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
                in eval (mut_rec 1 l env) (snd (List.nth l (i - 1)))
    end
    (* with Match_failure _ -> failwith "Unexpected Runtime Error : Match Failed" *)