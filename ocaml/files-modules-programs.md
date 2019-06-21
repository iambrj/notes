`List.Assoc.find <list-name> <key>` returns the value corresponding to `<key>`

`List.Assoc.add <list-name> <key> <value>` allocates a new list with 
`<key> <value>` pair added

`let () = ` serves the purpose of `main` from C/C++

If `Core` or any other external libraries are not being used, the executable can
be built using
```
ocamlc freq.ml -o freq.byte
```

When using external libraries etc, use
```
ocamlfind ocamlc -linkpkg -thread -package core freq.ml -o freq.byte
```

A module is a collection of definitions that are stored within a namespace

A module defined by a file `filename.ml` can be constrained by a signature
placed in a file called `filename.mli`

`val` declarations are used to specify values in a signature. Syntax
```
val <identifier> : <type>
```

A type is abstract if its name is exposed in the interface, but its definition
is not.
