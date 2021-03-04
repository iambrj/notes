open Base

include (module type of Option)

val apply : ('a -> 'b) t -> 'a -> 'b t
