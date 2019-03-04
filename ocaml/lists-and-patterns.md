An OCaml list is an immutable, finite sequence of elements of the same type.
OCaml lists can be generated using a bracket-and-semicolon notation:
```
[1;2;3;];;
```
And they can also be generated using the equivalent :: notation:
```
1 :: (2 :: (3 :: []));;
1 :: 2 :: 3 :: [];;
```
:: operator is right-associative

[] is polymorphic - it can be used with elements of any type.

Use :: to extract elements from a list
```
let rec sum l = 
	match l with
	| [] -> 0
	| hd :: tl -> hd + sum tl
;;
```
Use _ for denoting anything (wildcard).

List.map takes a list and a function for transforming elements of that list,
and returns a new list with the transformed elements.
```
List.map ~f:String.length ["Hello"; ""World!];
- : int list = [5; 6]
```
List.map2_exn takes two lists and a function for combining them (exn indicates
exception on lists with mismatched lengths)
```
List.map2_exn ~f:Int.max [1; 2; 3] [3; 2; 1]
- : int list = [3; 2; 3]
```

List.fold takes three arguments: a list to process, an initial accumulator
value, and a function for updating the accumulator. List.fold walks over the
list from left to right, updating accumulator at each step and returning the
final value of the accumulator when it's done.
```
List.fold ~init:0 ~f:(+) [1;2;3;4];;
- : int = 10
``
List.reduce is a specialized version of List.fold that doesn't require an 
explicit starting value, and whose accumulator has to consume and produce
values of the same type as the elements fo the list it applies to. Its 
signature is `'a list -> f:('a -> 'a -> 'a) -> 'a option = <fun>`
```
List.reduce ~f:(+) [1;2;3;4;5]
- : int option = Some 15
List.reduce ~f:(+) []
- : int option = None
```
List.filter lets you select a certain elements from a list. It takes two 
arguments - a predicate f and a list
```
List.filter ~f:(fun x -> x mod 2 = 0) [1;2;3;4;5];;
- : int list [2; 4]
```
List.dedup removes duplicates from a list.

List.partition_tf takes a list and a function for computing a Boolean condition
on the list elements and returns two lists
```
let is_ocmal_source s = 
	match String.rsplit2 s ~on:'.' with
	| Some (_,("ml"|"mli")) -> true
	| _ -> false
;;
let (ml_files, other_files) = 
	List.partition_tf(Sys.ls_dir ".") ~f:is_ocaml_source;;
val ml_files : string list = ["example.mli"; "example.ml"]
val other_files : string list = ["main.topscript"; "lists_layout.ascii"]
```

List.append and @ operator concatenate pairs of lists:
```
List.append [1;2;3] [4;5;6];;
[1;2;3] @ [4;5;6];;
- : int list = [1; 2; 3; 4; 5; 6]
```

A tail recursive if all of its recursive calls are tail calls.

An invocation is considered a tail call when the caller doesn't do anything
with the value returned by the callee except to return it.

A `when` clause allows us to add an extra precondition to a pattern in the form
of an arbitary OCaml expression.
```
let rec destutter = function
	| [] | [_] as l -> l
	| hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
	| hd :: tl -> hd :: destutter tl
;;
```
