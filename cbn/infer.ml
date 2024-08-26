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
        | CRLet (x,e) -> 
            let a = new_ty_var ()
            in let (t, tenv') = infer_expr ((x,TVar a)::tenv) e
            in ([(x,t)], (x,t)::tenv')
        | CMRLet l -> 
            let types = List.map (fun (f,_) -> (f, TVar (new_ty_var ()))) l
            in let crs = List.concat (List.mapi (fun i (_,e) ->
                let (t2) = snd (List.nth types i)
                in let (t, c) = gather_ty_constraints (types@tenv) e
                in (t,t2)::c) l)
            in let s = ty_unify crs
            in let res = List.mapi (fun i (f,_) ->  
                (f, apply_ty_subst s (snd (List.nth types i)))) l
            in (res, List.map (fun (x,t) -> (x, apply_ty_subst s t)) (res@tenv))