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

# Module declaration syntax 

Syntax: `module <name> : <signature> = <implementation>`.
Example:
```
open Base
module Time = Core_kernel.Time

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val (=) : t -> t -> bool
end

module String_id = struct
  type t = string
  let of_string x = x
  let to_string x = x
  let (=) = String.(=)
end

module Username : ID = String_id
module Hostname : ID = String_id

type session_info = { user: Username.t;
                      host: Hostname.t;
                      when_started: Time.t;
                    }
```
# Opening modules
- Local open has two synatxes
```
let average x y =
    let open Int64 in
    (x + y) / of_int 2;;

let averga x y
    Int64.((x + y) / of_int 2);;
```
- Names can also be rebinded locally
```
let print_median m =
    match m with
    | Counter.Median string -> printf "True median: %s\n" string
    | Counter.Before_and_after (b, a) -> printf "Before after: %s %s\n" b a

let local_print_median m =
    let module C = Counter in
    match m with
    | C.Median string -> printf "True median: %s\n" string
    | C.Before_and_after (b, a) -> printf "Before after: %s %s\n" b a
```
