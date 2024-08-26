%{
  open Syntax
  
(* type <Syntax.command list> commands
  commands : 
  | command commands {$1 :: $2 }
  
  로 처리하면 커맨드를 전부 입력받은 뒤에 해결하므로 불편 *)

%}

// not everything is possible
// 1 + let x = 1 in x;; -> parse_error

%token <int>    INT
%token <string> ID 
%token TRUE FALSE
%token LET REC AND IN
%token FUN RARROW
%token MATCH WITH END BAR
%token UNDERSCORE
%token IF THEN ELSE
%token COMMA
%token PLUS MINUS MUL DIV
%token EQ LT
%token DCOLON LBRK RBRK SEMI
%token LPAR RPAR 
%token EOF DSEMI

%start main 
%type <Syntax.command> main

%% 

main:
  | comm                { $1 }
  | EOF                 { raise End_of_file }
;

comm:
  | e=expr DSEMI              { CExp e }
  | LET x=var e=eq_fun_expr DSEMI          { CLet (x, e)}
  | LET REC f=var x=var e=eq_fun_expr DSEMI  { CRLet (f, x, e) }
  | LET REC f=var x=var e=eq_fun_expr AND l=mutual_rec_fun DSEMI  { CMRLet ((f, x, e) :: l) }
;
expr:
  | IF e0=expr THEN e1=expr ELSE e2=expr                { EIf (e0,e1,e2) }
  | LET x=var e1=eq_fun_expr IN e2=expr                 { ELet (x ,e1, e2) }
  | LET REC f=var x=var e1=eq_fun_expr IN e2=expr       { ERLet (f, x, e1, e2) }
  | LET REC f=var x=var e1=eq_fun_expr AND l=mutual_rec_fun IN e2=expr  { EMRLet (((f, x, e1) :: l), e2) }
  | FUN e=fun_expr                                      { e }
  | MATCH e=expr WITH l=match_expr END                  { EMatch (e, l) }  
  | e1=comp_expr COMMA e2=tuple_expr                    { ETuple (e1, e2) } 
  | MINUS e=expr                                        { ENeg e } // todo: minus simple_expr
  | e=comp_expr                                         { e }
;

mutual_rec_fun:
  | f=var x=var e=eq_fun_expr AND l=mutual_rec_fun { (f, x, e) :: l }
  | f=var x=var e=eq_fun_expr                      { (f, x, e) :: [] }
;

eq_fun_expr:
  | x=var e=eq_fun_expr           { EFun (x, e) }
  | EQ e=expr                   { e }
;

fun_expr:
  | x=var e=fun_expr              { EFun (x, e) }
  | x=var RARROW e=expr           { EFun (x, e) }
;

match_expr:
  | p=pattern RARROW e=expr       { [(p, e)] }
  | p=pattern RARROW e=expr BAR l=match_expr { (p, e) :: l }
;

pattern:
  | p1=list_pattern COMMA p2=tuple_pattern      { PTuple (p1, p2) } 
  | p=list_pattern                              { p } 
;

tuple_pattern:
  | p1=list_pattern COMMA p2=tuple_pattern       { PTuple (p1, p2) }
  | p=list_pattern                               { PTuple (p, PValue (VNil)) }
;

list_pattern:
  | p1=simple_pattern DCOLON p2=list_pattern   { PList (p1, p2) }
  | LBRK p=simplified_pattern_list RBRK        { p }
  | p=simple_pattern                           { p }
;

simplified_pattern_list:
  | p1=pattern SEMI p2=simplified_pattern_list { PList (p1, p2) }
  | p=pattern                              { PList (p, PValue (VNil)) }
;

simple_pattern:
  | TRUE               { PValue (VBool true)}
  | FALSE              { PValue (VBool false)}
  | i=INT              { PValue (VInt i) }
  | LBRK RBRK          { PValue (VNil) }
  | x=ID               { PVar   x }
  | UNDERSCORE         { PWild }
  | LPAR p=pattern RPAR { p }
;

tuple_expr:
  | e1=comp_expr COMMA e2=tuple_expr   { ETuple ( e1, e2) } 
  | e=comp_expr                        { ETuple ( e, EValue VNil) } 
;

comp_expr:
  | e1=comp_expr EQ e2=list_expr   { EBin (OpEq, e1, e2) } 
  | e1=comp_expr LT e2=list_expr   { EBin (OpLt, e1, e2) } 
  | e=list_expr                    { e } 
;

list_expr:
  | e1=arith_expr DCOLON e2=list_expr   { EList (e1, e2) }
  | e=arith_expr                        { e } 
;

arith_expr:
  | e1=arith_expr PLUS e2=factor_expr  { EBin (OpAdd,e1,e2) }
  | e1=arith_expr MINUS e2=factor_expr { EBin (OpSub,e1,e2) }
  | e=factor_expr                      { e }
;

factor_expr: 
  | e1=factor_expr MUL e2=app_expr { EBin (OpMul,e1,e2) }
  | e1=factor_expr DIV e2=app_expr { EBin (OpDiv,e1,e2) }
  | e=app_expr                     { e }
;

app_expr:
  | e1=app_expr e2=atomic_expr       { EApp (e1, e2) }
  | e=atomic_expr                   { e }
;


atomic_expr:
  | TRUE             { EValue (VBool true) }
  | FALSE            { EValue (VBool false) }
  | i=INT            { EValue (VInt i) }
  | x=var            { EVar x}
  | LPAR e=expr RPAR { e }
  | LBRK e=simplified_expr_list RBRK { e }
;

simplified_expr_list:
  | e1=expr SEMI e2=simplified_expr_list   { EList (e1, e2) }
  | e=expr                                 { EList (e, EValue (VNil)) }
  |                                        { EValue (VNil) }

;

var:
  | ID  { $1 } 
;