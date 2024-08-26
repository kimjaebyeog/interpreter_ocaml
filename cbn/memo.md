# call-by-name variation

use thunk 

there is no `value = ... | VThunk of thunk` because it requires to implement force evaluation

idea : 
```ocaml
EAPP(e1,e2) -> (force eval e1)  (eval e2)
let eval ,...
and force : value -> value = fucntion
    VThunk (expr, env) -> force (eval env expr)
    | x -> x
EMatch (p,e) -> match p with
    PVar x -> (x,VThunk)
    PVal v -> if (force v = force (eval e env))
    ...
```

## todo

- disallow let rec x = x