open Base
open Stdio

type basic_color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

type weight =
    | Regular
    | Bold

type color =
    | Basic of basic_color * weight
    | RGB of int * int * int
    | Gray of int

let basic_color_to_int = function
    | `Black -> 0
    | `Red -> 1
    | `Green -> 2
    | `Yellow -> 3
    | `Blue -> 4
    | `Magenta -> 5
    | `Cyan -> 6
    | `White -> 7

let color_to_int = function
    | `Basic (basic_color, weight) ->
            let base = match weight with `Bold -> 8 | `Regular -> 0 in
            base + basic_color_to_int basic_color
    | `RGB (r, g, b) -> 16 + b + g * 6 + r * 36
    | `Gray i -> 232 + i

(* Beware, tuple syntax looks awfully similar to type constructor syntax *)
let color_purple = RGB (200, 0, 200);;
let purple = (200, 0, 200);;
(* `RGB purple` doesn't work! *)

module Time_ns = Core_kernel.Time_ns
module Log_entry = struct
    type t =
        {
            session_id : string;
            time : Time_ns.t;
            important : bool;
            message : string;
        }
end

module Heartbeat = struct
    type t =
        {
            session_id : string;
            time : Time_ns.t;
            status_message : string;
        }
end

module Logon = struct
    type t =
        {
            session_id : string;
            time : Time_ns.t;
            user : string;
            credentials : string
        }
end

(* Use variant to package these three types together *)
type client_message =
    | Logon of Logon.t
    | Heartbeat of Heartbeat.t
    | Log_entry of Log_entry.t

(* Variants represent differences between different cases, records represent
 * shard structure *)

(* Records can also be directly embedded into a variant, although now they
 * cannot be passed outside *)

(* variants can be recursive *)
type 'a expr =
    | Base of 'a
    | Const of bool
    | And of 'a expr list
    | Or of 'a expr list
    | Not of 'a expr

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field : mail_field; contains : string }

let test field contains = Base { field; contains };;

let rec eval expr base_eval =
    let eval' expr = eval expr base_eval in
    match expr with
    | Base base -> base_eval base
    | Const bool -> bool
    | And exprs -> List.for_all exprs ~f:eval'
    | Or exprs -> List.exists exprs ~f:eval'
    | Not expr -> not (eval' expr)
;;

let and_ l =
    if List.exists l ~f:(function Const false -> true | _ -> false)
    then Const false
    else
        match List.filter l ~f:(function Const true -> false | _ -> true) with
        | [] -> Const true
        | [ x ] -> x
        | l -> And l
;;

let or_ l =
    if List.exists l ~f:(function Const true -> false | _ -> true)
    then Const true
    else
        match List.filter l ~f:(function Const false -> true | _ -> false) with
        | [] -> Const true
        | [ x ] -> x
        | l -> Or l
;;

let not_ = function
    | Const b -> Const (not b)
    | e -> Not e
;;

let rec simplify = function
    | Base _ | Const _ as x -> x
    | And l -> and_ (List.map ~f:simplify l)
    | Or l -> or_ (List.map ~f:simplify l)
    | Not e -> not_ (simplify e)
;;

(* OCaml also has polymorphic variants *)
let three = `Int 3;;
let four = `Float 4.;;
let nan = `Not_a_number;;

[three; four; nan];;

let five = `Int "five";;
(* Following throws a type error! *)
(* [three; four; five];; *)

(* Lower bounded polymorphism, since is_positive cannot deal with values with
 * tags other than these *)
let is_positive = function
  | `Int   x -> x > 0
  | `Float x -> Float.(x > 0.)
;;

(* Exact polymorphism is when both < and > match *)
let exact = List.filter ~f:is_positive [three;four];;

(* Upper and lower bounds can be different! *)
let is_positive = function
  | `Int   x -> Ok (x > 0)
  | `Float x -> Ok Float.(x > 0.)
  | `Not_a_number -> Error "not a number"
;;

List.filter [three; four] ~f:(fun x ->
match is_positive x with Error _ -> false | Ok b -> b)
;;

type extended_color =
    | Basic of basic_color * weight
    | RGB of int * int * int
    | Gray of int
    | RGBA of int * int * int * int
;;

(* Can use polymorphic variants to reuse code and enforce equality of tags
 * between variants *)
let extended_color_to_int = function
    | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    | (`Basic _  | `RGB _  |`Gray _  ) as color -> color_to_int color
;;
