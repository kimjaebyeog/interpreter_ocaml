(** name : name of variable  *)
type name = string

(** binOp : binary operation [+,-,*,/,=,<]  *)
type binOp = OpAdd | OpSub | OpMul | OpDiv | OpEq | OpLt

(** value : num, true, false *)
(* There is no not-recursive mutual function *)
type value = VInt of int | VBool of bool
| VFun of name * expr * env
| VRFun of name * name * expr * env
| VMRFun of int * ((name * name * expr) list) * env
| VCons of value * value
| VTuple of value * value
| VNil

and env = (name * value) list

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
  | ERLet of name * name * expr * expr
  | EMRLet of ((name * name * expr) list) * expr
  | EMatch of expr * ((pattern * expr) list)
  | EList of expr * expr
  | ETuple of expr * expr
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
  | CRLet of name * name * expr
  | CMRLet of (name * name * expr) list

(** command result : (((bound)varname, (evaled)value)list newenv)  *)
type command_result = Command_result of ((name option * ty * value) list) * env * ty_env