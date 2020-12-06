open Stdio
open Base

 (* let bindings can pattern match on LHS *)
let (ints, strings) = List.unzip [(1, "one"); (2, "two"); (3, "three")];;

(* lambda functions *)
(fun x -> x + 1) 5;;

(* let bindings a function *)
let add1 x = x + 1 in (add1 5);;
(* is just syntactic sugar for *)
let add1 = (fun x -> x + 1) in (add1 5);;

(* use and to define mutually recursive functions *)
let rec is_even x =
    if x = 0 then true else is_odd (x - 1)
and is_odd x =
    if x = 0 then false else is_even (x - 1);;

(* use function to exploit built in pattern matching *)
let head_or_minus1 = function
    | x :: xs -> x
    | [] -> -1;;

let square x = x * x;;

(* prefix argument name with ~ to make it labelled *)
let calc ~add ~sub ~mul ~init = init + add - sub * mul;;
calc ~init:3 ~sub:5 ~mul:2 ~add:10;;

(* label punning *)
let init = 3 in
let sub = 5 in
let mul = 2 in
let add = 10 in
calc ~sub ~mul ~add ~init;;

(* sexy usecase - do partial application without argument flipping business *)

(* prefix argument name with ? to make it an option *)
(* gotcha: optional args go before other args *)
let optional_add ?y x =
    let y = match y with
    | None -> 0
    | Some z -> z
    in x + y;;
