open Stdio
open Base

let x : int = 3;;
let y = 4;;

let int_equality x y =
    x = y;;

let bool_equality x y =
    x = y;;



printf "%d\n" (3_000_0_00);; (* use _ *anywhere* inside number to incrase readbility *)
printf "%d\n" (x + y);;
printf "%d\n" (x * y);;
printf "%f\n" (5.0 *. 2.0);; (* note dot after star, different operators for different types *)

printf "ratio : %f\n" (Float.of_int 11 /. Float.of_int 7);;

printf "%B\n" (int_equality 1 1);;
printf "%B\n" (int_equality 1 2);;
printf "List has 5 : %B\n" (List.exists [1;2;3] (int_equality 5));; (* whoa functions are curried! *)
printf "List has 5 : %B\n" (List.exists [1;2;3;5] (int_equality 5));;

let () =
    let open Float.O in (* locally open module *)
    printf "pi : %f\n" (22.0 / 7.0);;

let first_tuple = (1, "tuple")
let second_tuple = (2., "tuple")

let (a, b) = first_tuple;; (* pattern matching*)
    printf "First component is : %d\n" a;;
    printf "Second component is : %s\n" b;;

let first3numbers = [1;2;3]

let rec intListPrinter l =
    match l with
    | [] -> printf "\n"
    | x :: xs -> (printf "%d; " x) |> fun () -> intListPrinter xs (* monads! *)
;;

intListPrinter first3numbers;;
intListPrinter (List.map first3numbers (fun x -> x * x));;
intListPrinter (List.map first3numbers ~f:(fun x -> x * x));;
intListPrinter ([1;2;3] @ [4;5;6]);;

let safeIntDivide a b = if b = 0 then None else Some (a / b);;
printf "10/2 = %d\n" (match (safeIntDivide 10 2) with
                   | None -> 99999999
                   | Some x -> x);;
printf "10/0 = %d\n" (match (safeIntDivide 10 0) with
                   | None -> 99999999
                   | Some x -> x);;

(* OCaml has records as well! *)
type footwear2 = { kind : string; cost : float; brand : int };;
type footwear = { kind : string; cost : float; brand : int };;
let home = {kind = "slipper"; cost = 200.; brand = 0};;
let school = {kind = "shoe"; cost = 1000.; brand = 1};;
let sports = {kind = "basketball"; cost = 2000.; brand = 2};;

type point2d = {x : float; y : float};;
type circle = {c : point2d; r : float};;
type rect = {lower_left : point2d; height : float; width : float};;
type segment = {p1 : point2d; p2 : point2d};;

(* OCaml also has variants, aka sum types!*)
type element2d =
    | Circle of circle
    | Rect of rect
    | Segment of segment
;;

let first_element = Circle {c = {x = 10.; y = 20.}; r = 5.};;

let which_element e =
    match e with
    | Circle circ -> printf "Circle with center (%f, %f) and radius %f" circ.c.x circ.c.y circ.r
    | Rect r -> printf "Rectangle with (height, width) = (%f, %f) and lower_left (%f, %f)" r.height r.width r.lower_left.x r.lower_left.y
    | Segment s -> printf "Segment with (p1, p2) = ((%f, %f), (%f, %f))" s.p1.x s.p1.y s.p2.x s.p2.y
;;

(* Of course OCaml has arrays! *)
let languages = [|"OCaml"; "Haskell"; "Scheme"; "Racket"; "C"; "Java"|];;
(* Accessing array element *)
printf "Second langauge is %s\n" languages.(1);;
(* updating array element *)
languages.(1) <- "Coq";;
printf "Second langauge is %s\n" languages.(1);;

type phone = {mutable price : float; brand : string};;
let brj_phone = {price = 7000.; brand = "mi"};;
brj_phone.price <- 5000.;;

(* ref type, to simulate pointers. Record with single variant *)
let x = {contents = 1};;
(* you can dereference a pointer! *)
printf "x has %d\n" !x;;
(* you can update a pointer's contents! *)
x := 5;;
printf "x now has %d\n" !x;;
