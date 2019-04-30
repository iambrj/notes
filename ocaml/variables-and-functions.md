A variable is an identifier whose meaning is bound to a particular value.

```
let <variable> = <expr>;;
```

Variable names ***must*** start with a lowercase letter or an underscore.
Every variable binding has a scope, which is the portion of code that can
refer to that binding. Scope of a variable can be limited as follows

```
let <variable> = <expr1> in <expr2>;;
```

An inner binding shadows/hides an outer binding.

`let` bindings are immutable (they cannot be changed)

`let` bindings support pattern matching

```
let (ints,strings) = List.unzip [(1, "One"); (2, "Two"); (3, "Three")];;
(* List.unzip converts a list of pairs into a pair of lists *)
```

An anonymous function is a function that is declared without being named. They
are declared using the `fun` keyword

```
(fun x -> x+1);;
```

To apply an anonymous function to an argument

```
fun( x -> x + 1) 7
- : int = 8
;;
```

`let` bindings can be used to name anonymous functions

```
let plusone = (fun x -> x + 1);;
```

Syntatic sugar for naming

```
let plusone x = x + 1;;
```

Multiargument functions

```
let abs_diff x y = abs(x - y);;
```

which is ofcourse, syntactic sugar for Currying

```
let abs_diff = (fun x -> (fun y -> abs (x - y)));;
```

`->` is right associative.

Partial application - apply some of the arguments of a curried 
function to get a new function.

A function is recursive if it refers to itself in its definition.

To define a recursive function, use `let` in conjunction with `rec`

```
let rec find_first_stutter list = 
	match list with
	| [] | [_] ->
	| x :: y :: tl ->
		if(x = y) then Some x else find_first_stutter (y :: tl)
;;
```

Use `let` and `rec` in conjunction with `and` to create mutually recursive
functions

```
let rec is_even x = 
	if x = 0 then true else is_odd (x - 1)
and is_odd x = 
	if x = 0 then false else is_even (x - 1)
;;

```

Recursive functions must be specified with `rec` for the type-inference
algorithm.

A function is treated syntactically as an operator if the name of that function
is chosen from one of the following

```! $ % & + - . / : < = > ? @ ^ | ~```

To redefine the meaning of a built-in operator use the following syntax

```
let (+) x y = x + y;;
```

***NOTE:*** Associativity of an operator cannot be changed - only its definition can. 

Another way to define a function is to use the `function` keyword.

```
let some_or_zero = function
	| Some x -> x
	| None -> 0
;;
```

Labeled arguments allow you to identify function argument by name

```
let ratio ~num ~denom = float num /. float denom;;
ratio ~denom:10 ~num:3;;
(\* label punning \*)
let num = 3 in
	let denom = 4 in
		ratio ~num ~denom;;
(\* syntactic sugar for \*)
let concat ?(sep="") x y = x ^ sep ^ y;;
```
In higher order context (passing a function with labeled arguments to another
function) order matters
```
let apply_to_tuple f (first,second) = f ~first ~second;;
let apply_to_tuple_2 f (first,second) = f ~second ~first;;
let divide ~first ~second = first / second;;
apply_to_tuple divide (3,4);; (\* works \*) 
apply_to_tuple_2 divide (3,4);; (\* does not work \*) 
```

An optional argument is a labeled argument that the caller can choose whether
or not to provide. It is syntactic sugar for passing None when the caller does
not provide an argument and Some when done.
```
let concat ?sep x y = 
	let sep = match sep with None -> "" | Some x -> x in
		x ^ sep ^ y
;;
```
Note the use of `?` to mark an argument as optional

Type inference
```
let numeric_deriv ~delta ~x ~y ~f =
	let x' = x +. delta in
	let y' = y +. delta in
	let base = f ~x ~y in
	let dx = (f ~x:x' ~y -. base) /. delta in
	let dy = (f ~x ~y:y' -. base) /. delta in
	(dx,dy)
;;
(\* Does not compile - use explicit type annotation \*)
```
An optional argument is erased as soon as the first positional (i.e. neither
labeled nor optional) argument defined after the optional argument is passed in.
```
let concat x ?(sep="") y = x ^ sep ^ y;;
concat "a" "b" ~sep:"=";;
```
An optional argument that doesn't have any following positional arguments can't
be erased.
