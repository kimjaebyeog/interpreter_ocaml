open Syntax
open Ty

let infer_expr : ty_env -> expr -> ty * ty_env
    = fun tenv e ->
    let (t, c) = gather_ty_constraints tenv e
    in let s = ty_unify c
    in (apply_ty_subst s t, List.map (fun (x,t) -> (x, apply_ty_subst s t)) tenv)

let infer_cmd : ty_env -> command -> ty_env * ty_env
    = fun tenv cmd ->
    match cmd with
        CExp e -> 
            let (t, tenv') = infer_expr tenv e
            in ([("",t)],tenv')
        | CLet (x,e) -> 
            let (t, tenv') = infer_expr tenv e
            in ([(x,t)], (x,t)::tenv')
        | CRLet (f,x,e) -> 
        (* can't it be calced in ty.ml? *)
        (* !!! 수정 필수 !!! infer_expr 를 사용하면 tenv에 x가 남으므로 안됨 *)
        (* 선두에 있는 (x,_) 만 제거? *)
            let a = new_ty_var ()
            in let b = new_ty_var ()
            in let (_, tenv') = infer_expr ((x, TVar a)::(f, TFun(TVar a,TVar b))::tenv) e
            in let (tx, _) = infer_expr tenv' (EVar f)
            (* 위의 줄 대신 여기서 직접 f의 type에 subsitution을 적용시키면 뒤에 TFun...이라 적을 필요가 없음*)
            in ([(f,tx)], (f, tx)::tenv')
        | CMRLet l -> 
        (* let rec f ... and g...
         는 let (f, g) = let ...and... in (f, g)로 표현하가능하므로 
         infer_expr 을 직접 이용 가능 *)
        (* ocaml의 let x = ...는 기본적으로 x가 패턴으로 되어있음 따라서 이것저것 가능 *)
            let fun_types = List.map (fun (f,_,_) -> (f, TFun (TVar (new_ty_var ()), TVar (new_ty_var ())))) l
            in let crs = List.concat (List.mapi (fun i (_,x,e) ->
                let t1, t2 = begin match snd (List.nth fun_types i) with
                  TFun (t1,t2) -> t1,t2
                  |_ -> assert false
                end
                in let (t, c) = gather_ty_constraints ((x, t1)::fun_types@tenv) e
                in (t,t2)::c) l)
            in let s = ty_unify crs
            in let res = List.mapi (fun i (f,_,_) ->  
                (f, apply_ty_subst s (snd (List.nth fun_types i)))) l
            in (res, List.map (fun (x,t) -> (x, apply_ty_subst s t)) (res@tenv))


        (* (* This doesn't work *)
            let fun_types = List.map (fun (f,_,_) -> (f, TFun (TVar (new_ty_var ()), TVar (new_ty_var ())))) l
            in let res_n_tenv = (List.mapi (fun i (f,x,e) ->
                let (TFun (t1, t2)) = snd (List.nth fun_types i)
                in let (t, tenv') = infer_expr ((x, t1)::fun_types@tenv) e
                in let (tx, _) = infer_expr tenv' (EVar x)
                in ((f,TFun (tx, t)), (f, TFun (tx, t))::tenv')) l)
            in (List.map fst res_n_tenv, List.concat (List.map snd res_n_tenv)) *)
        (* | _ -> raise Ty_error *)

(* infer_expr [] (ELet (x, (EValue (VInt 1)), (EApp (EFun (x, (EValue (VInt 3))), EVar x))));; *)
