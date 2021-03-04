open Base

(* one way to define abstract type - expose name, not definition *)
type t

val empty : t

(* touch increments count of line by one *)
val touch : t -> string -> t

val to_list : t -> (string * int) list
