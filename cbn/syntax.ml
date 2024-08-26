(** name : name of variable  *)
type name = string

(** binOp : binary operation [+,-,*,/,=,<]  *)
type binOp = OpAdd | OpSub | OpMul | OpDiv | OpEq | OpLt

(** value : num, true, false *)
(* to add VThunk, we need to implement force eval *)
type value = VInt of int | VBool of bool
| VFun of name * expr * env
| VCons of thunk * thunk
| VTuple of thunk * thunk
| VNil

(* thunk is baisically function without argument, we dont need argument to eval and just postpone the evaluation until it is used *)
(* do we actually need env for thunk?? *)
and thunk = 
  Thunk of expr * env
  | RThunk of name * expr * env
  | MRThunk of int * ((name * expr) list) * env

and env = (name * thunk) list

(** expression *)
and expr = 
  EValue of value
  | EVar of name
  | ENeg of expr
  | EBin of binOp * expr * expr
  | EIf of expr * expr * expr
  | ELet of name * expr * expr
  | EFun of name * expr
  | EApp of expr * expr  
  | ERLet of name * expr * expr
  | EMRLet of ((name * expr) list) * expr
  | EMatch of expr * ((pattern * expr) list)
  | EList of expr * expr
  | ETuple of expr * expr
  | EThunk of thunk
and pattern = PValue of value | PVar of name
  | PList of pattern * pattern
  | PTuple of pattern * pattern
  | PWild

type ty_var = int
type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TVar of ty_var
  | TList of ty
  | TTuple of ty * ty 
  | TNil
type ty_subst = (ty_var * ty) list
type ty_constraints = (ty * ty) list
type ty_env = (name * ty) list


(** command *)
type command = 
  CExp of expr
  | CLet of name * expr
  | CRLet of name * expr
  | CMRLet of (name * expr) list

(** command result : (((bound)varname, (evaled)value)list newenv)  *)
(* todo *)
(* type command_result = Command_result of ((name option * ty * (value option)) list) * env * ty_env *)
type command_result = Command_result of env * ty_env