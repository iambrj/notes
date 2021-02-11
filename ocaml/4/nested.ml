open Base
open Stdio
module Time = Core_kernel.Time

(* also possible to have different implementations for the same signature! *)
module type MySet = sig
    type set
    type elt
    val insert_set : set -> elt -> set
    val lookup_set : set -> elt -> bool
    val to_list : set -> elt list
    val from_list : elt list -> set
end

module List_string_set = struct
    type set = List
    type elt = string
    let rec insert_set =
        fun s e -> match s with
        | [] -> [e]
        | h :: t ->
                if String.(=) e h
                then s
                else h :: (insert_set t e)
    let rec lookup_set =
        fun s e -> match s with
        | [] -> false
        | h :: t -> (String.(=) e h) || lookup_set t e
    let to_list x = x
    let from_list x = x
end

module LSS : MySet = List_string_set

module Username : sig
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
