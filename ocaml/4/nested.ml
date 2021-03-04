open Base
open Stdio
module Time = Core_kernel.Time

module type Username = sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val (=) : t -> t -> bool
end

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

type session_info = {   user : Username.t;
                        host : Hostname.t;
                        when_started : Time.t;
                    }

let sessions_have_same_user s1 s2 =
    Username.(=) s1.user s2.user

(* also possible to have different implementations for the same signature! *)
(* module types is analogous to Haskell type classes *)
(* bunch of declarations that any instance is obligated to implement*)
module type MySet = sig
    type set
    type elt
    val insert_set : set -> elt -> set
    val lookup_set : set -> elt -> bool
    val to_list : set -> elt list
    val from_list : elt list -> set
end

(* actual implementation/instance *)
module List_string_set = struct
    type elt = string
    type set = elt list
    let insert_set s e =
        if List.mem s e String.(=)
        then s
        else e :: s
    let rec lookup_set =
        fun s e -> match s with
        | [] -> false
        | h :: t -> (String.(=) e h) || lookup_set t e
    let to_list x = x
    let from_list x = x
end

module LSS : MySet = List_string_set
