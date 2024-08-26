open Syntax
open Error
(* newty 에 incr을 사용하는게 좋을것 *)



(* todo : tyschema 도입 (ty_var list * ty)
    (원래는)tenv 에 ty 대신 tyschema가 들어감
    원래의 ty는 ([],ty)로 표현
    각종 연산시 FV에 대한 체크가 필요함 *)

let tv : int ref = ref 0

let new_ty_var : unit -> int
    = fun  ()-> incr tv; !tv
(* how to get rid of used tv?*)

(** apply_ty_subst
 goes through all ty_subst sequnce *)
let rec apply_ty_subst : ty_subst -> ty -> ty
    = fun s t ->
    match t with
    TVar i1 -> begin
        match s with
        [] -> t
        | (i2, t2) :: rest ->
            if i1 = i2 then apply_ty_subst rest t2 else apply_ty_subst rest t
        end
    | TFun (t11, t12) -> (* apply to both types *)
        TFun (apply_ty_subst s t11, apply_ty_subst s t12)
    | TList t -> 
        TList (apply_ty_subst s t)
    | TTuple (t11, t12) -> 
        TTuple (apply_ty_subst s t11, apply_ty_subst s t12)
    | _ -> t


(* (** compose two type substitutions
assume each subst are not cyclic
*)
let rec compose_ty_subst : ty_subst -> ty_subst -> ty_subst
    = ( @ ) *)

(** unify type constraints *)
let rec ty_unify : ty_constraints -> ty_subst
    = fun c ->
    let rec is_include : int -> ty -> bool
    (* occurs check *)
        = fun i t -> match t with
            TVar i' -> i = i'
            | TList t' -> is_include i t'
            | TFun (t1', t2') -> (is_include i t1') || (is_include i t2')
            | _ -> false
    in match c with
    [] -> []
    | (t1,t2) :: rest when t1 = t2 -> ty_unify rest
    | (TFun (t11, t12), TFun (t21, t22)) :: rest->
        ty_unify ((t11,t21)::(t12,t22)::rest)
    | (TList t1', TList t2') :: rest ->
        ty_unify ((t1',t2')::rest)
    | (TTuple (t11, t12), TTuple (t21, t22)) :: rest ->
        ty_unify ((t11,t21)::(t12,t22)::rest)
    | (TVar i1, t2) :: rest->
        if is_include i1 t2 then raise (Ty_error "T is cyclic({X = T} and T has X in it)")
        else 
            (i1, t2)::(ty_unify (List.map 
                (fun (a, b) -> (apply_ty_subst [(i1, t2)] a, apply_ty_subst [(i1, t2)] b)) 
                rest))
        (* why we dont need to cleanup substitution after unification?-> memo.appendix  *)
    | (t1, TVar i2) :: rest-> ty_unify ((TVar i2,t1)::rest)
    (* todo : alert the reason for unification faliure *)
    (* | (t1,t2)::_ -> (print_type t1);(print_string " does not match with");(print_type t2);  raise (Ty_error "Type Unification Failed") *)
    | _ -> raise (Ty_error "Type Unification Failed")

(* let rec ty_cleanup_subst : ty_subst -> ty_subst
    = fun c -> match c with
    [] -> []
    |(i,t)::rest -> (i,t)::(List.map (function (i2,t2) -> (i2, (apply_ty_subst [(i, t)] t2))) rest) *)


(** gather type constraints from expr
return type of expr and generated constraints 
*)
let rec gather_ty_constraints : ty_env -> expr -> ty * ty_constraints
    = let (@) = List.rev_append in (* todo: change each or better, use set *)
    fun tenv e ->
    match e with
    EValue v -> begin
        match v with
        VInt _ -> (TInt, [])
        | VBool _ -> (TBool, [])
        | VNil -> 
            let a = new_ty_var ()
            in (TList (TVar a), [])
        | _ -> raise (Ty_error "gathering constraints failed with EValue of non (literal or []) ")
            (* should be unable to reach? *)
    end
    | EVar x -> begin
        match List.assoc_opt x tenv with
        None -> raise (Ty_error ("var "^x^" is not in type environment"))
        (* change to unbound error? *)
        | Some t -> (t, [])
    end
    | ENeg (e1) -> let (t1,c1) = gather_ty_constraints tenv e1
        in (TInt, (TInt,t1)::c1)
    | EBin (OpAdd, e1, e2) | EBin (OpSub, e1, e2) | EBin (OpMul, e1, e2) | EBin (OpDiv, e1, e2) -> 
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TInt, (t1,TInt)::(t2,TInt)::c1@c2)
        (* todo : union of set c1 c2? *)
    | EBin (OpEq, e1, e2) | EBin (OpLt, e1, e2) -> 
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TBool, (t1,t2)::c1@c2)
    | EIf (e1, e2, e3) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in let (t3, c3) = gather_ty_constraints tenv e3
        in (t2, (t1, TBool)::(t2, t3)::c1@c2@c3)
    | ELet (x, e1, e2) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints ((x,t1)::tenv) e2
        in (t2, c1 @ c2)
    | EFun (x, e1) ->
        let a = new_ty_var ()
        in let (t, c) = gather_ty_constraints ((x,TVar a)::tenv) e1
        in (TFun (TVar a, t), c)
    | EApp (e1, e2) -> 
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in let a = new_ty_var ()
        in (TVar a, ((t1, TFun(t2, TVar a))::c1@c2))
    | ERLet (x, e1, e2) ->
        let a = new_ty_var ()
        in let (t1, c1) = gather_ty_constraints ((x, TVar a)::tenv) e1
        in let (t2, c2) = gather_ty_constraints ((x, t1)::tenv) e2
        in (t2, c1 @ c2)
    | EMRLet (l, e) ->
        (* list of types of function freshly generated *)
        let types : ty_env = List.map (fun (f,_) -> (f, TVar (new_ty_var ()))) l
        (* constraints gathered from each function bodies (flattened)*)
        in let cntns : ty_constraints = List.concat (List.mapi (fun i (_,e) ->
            let (t2) = snd (List.nth types i)
            in let (t, c) = gather_ty_constraints (types@tenv) e
            in (t,t2)::c) l)
        in let (t, c) = gather_ty_constraints (types@tenv) e
        in (t, cntns@c)
    | EMatch (e, l) ->
        (* type of pattern, new bound types, constraints *)
        let rec gather_ty_constraints_pattern : pattern -> ty * ty_env * ty_constraints
            = fun p -> match p with
            PValue (VInt _) -> (TInt, [], [])
            | PValue (VBool _) -> (TBool, [], [])
            | PValue (VNil) -> let a = new_ty_var ()
                in (TList (TVar a), [], [])
            | PVar x -> let a = new_ty_var ()
            (* 한 패턴 안에서 같은 변수를 두번 대입하면 안됨, todo : deal this at plist and ptuple*)
                in (TVar a, [(x, TVar a)], [])
            | PList (p1, p2) -> 
                let (t1, tenv1, c1) = gather_ty_constraints_pattern p1
                in let (t2, tenv2, c2) = gather_ty_constraints_pattern p2
                in (TList t1, tenv1@tenv2, (TList t1, t2)::c1@c2)
            | PTuple (p1, PValue VNil) -> 
                let (t1, tenv1, c1) = gather_ty_constraints_pattern p1
                in (TTuple (t1, TNil), tenv1, c1)
            | PTuple (p1, p2) -> 
                let (t1, tenv1, c1) = gather_ty_constraints_pattern p1
                in let (t2, tenv2, c2) = gather_ty_constraints_pattern p2
                in (TTuple (t1, t2), tenv1@tenv2, c1@c2)
            | PWild -> let a = new_ty_var ()
                in (TVar a, [], [])
            | PValue _ -> raise (Ty_error "should not be able to reach here (PValue with non literal values) ")
            (* | _-> raise Ty_error "constraint gather failed match failure from pattern expression"*)
        (* pattern type, expr type, constraints *)
        in let temp = List.map (fun (p, e) ->
            let (t', tenv', c') = gather_ty_constraints_pattern p
            in let (t, c) = gather_ty_constraints (tenv'@tenv) e
            in (t', t, c'@c)) l
        in let a = new_ty_var ()
        in let (t, c) = gather_ty_constraints tenv e
        in let pattern_type_constraints = List.map (fun (x,_,_) -> (x, t)) temp
        in let expr_type_constraints = List.map (fun (_,x,_) -> (TVar a, x)) temp
        in let extra_type_constraints = List.concat (List.map (fun (_,_,x) -> x) temp)
        in (TVar a, c@pattern_type_constraints@expr_type_constraints@extra_type_constraints)
        (* todo : optimize *)
    | EList (e1, e2) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TList t1, (TList t1, t2)::c1@c2)
    | ETuple (e1, EValue (VNil)) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in (TTuple (t1,TNil), c1)
    | ETuple (e1, e2) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TTuple (t1,t2), c1@c2)
    | EThunk (Thunk (e1, _)) -> gather_ty_constraints tenv e1
    | EThunk (RThunk (x, e1, _)) -> 
        let a = new_ty_var ()
        in gather_ty_constraints ((x,TVar a)::tenv) e1
    | EThunk (MRThunk (i, l, _)) ->
        (* list of types of function freshly generated *)
        let types : ty_env = List.map (fun (f,_) -> (f, TVar (new_ty_var ()))) l
        (* constraints gathered from each function bodies (flattened)*)
        in let cntns : ty_constraints = List.concat (List.mapi (fun i (_,e) ->
            let (t2) = snd (List.nth types i)
            in let (t, c) = gather_ty_constraints (types@tenv) e
            in (t,t2)::c) l)
        in let (t, c) = gather_ty_constraints (types@tenv) (snd (List.nth l i))
        in (t, cntns@c)

    (* | _ -> raise Ty_error "constraints gather failed : match failure" *)