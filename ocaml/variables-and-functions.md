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

`let` bindings are mutable (they cannot be changed)

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

-> is right associative.

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

Use `let` and `rec` in conjunction with `and` to 

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
ration ~denom:10 ~num:3;;
(\* label punning \*)
let num = 3 in
	let denom = 4 in
		ratio ~num ~denom;;
```

