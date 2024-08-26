## TODO

### urgent

- change implement of rlet, mrlet : to accept (let rec f = fun x -> f x)
- infer_expr in infer.ml
- comment( (* ... *) )
- inform why ty_unify failed (stringify two types)
- optimize : 

### mid

- remove match end : (need to deal with parser conflict)
- infer_cmd -> (res,tenv) : make res more specific, (instead of [("" | varname), ty], use [(None | Some varname), ty])
- module print
- List.rev_append instead of @ for constraints (little faster and doesn't need space(O(n)))
- use set for constarints instead of lists
( problem : duplicates )

- seperate VNIL with TUPLE NIL
- seperate PValue with Values

### later
- implement unbound error
- (in .ml codes) write with |> (for readability)
- implement call by need (from call by name): by memoizing when eval variables with thunks\
 -> return new env when eval\
 -> env = bind list; bind = BV of name * value | BT of name * thunk
-  force eval
- fail unreacherble errors (+ enrich msgs)

# appendix

## unification and constraints

- because substitution is sequencial and\ 
    substitute rest of the constraints \
    ty vars are all reduced 
```
    c[1=true, 2=1]
    -> s[1->true] :: u(c[2=true]) -> s[1->true; 2->true]
    c[1=true, 1=2]
    -> s[1->true] :: u(c[true=2]) -> s[1->true; 2->true]
    c[2=1, 1=true]
    -> s[2->1]:: u(c[1=true]) -> s[2->1;1->true]
    c[1=2, 1=true]
    -> s[1->2]:: u(c[2=true]) -> s[1->2;2->true]
```
    all of those have same result
## naming convention

- constraints : cntn
- context : ctx


## miscellaneous

### vscode hacks
```json
{
  "key": "ctrl+k ctrl+d",
  "command": "editor.action.moveSelectionToNextFindMatch",
  "when": "editorFocus"
}
```

### use ocamldebug with dune

first, build bytecode with dune
```dune
(executable
 (public_name main)
 (name main)
 (modes byte exe))
```

then go to where bc is located (_build/...)and run ocamldebug
```
# ocamldebug main.bc
```

you should refer module name with wrapped name by dune
> The solution seems to refer to the `main` module as `dune__exe__Main` and it should be better in dune 3.0.\
> https://discuss.ocaml.org/t/using-ocamldebug-with-dune/7878

alternatively, 
> `(wrapped_executables false) ` on dune-project