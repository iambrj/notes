open Base

module Interval = struct
    type t = | Interval of int * int
             | Empty

    let create low high =
        if high < low then Empty else Interval (low, high)
end;;

module Extended_interval_i = struct
    include Interval

    let contains t x =
        match t with
        | Empty -> false
        | Interval (low, high) -> x >= low && x <= high
end;;

module Extended_interval_o = struct
    open Interval (* include is not same as open! *)

    let contains t x =
        match t with
        | Empty -> false
        | Interval (low, high) -> x >= low && x <= high
end;;

(* gives true *)
Extended_interval_i.contains (Extended_interval_i.create 3 10) 4;;

(* doesn't work, Extended_interval_o doesn't have create *)
Extended_interval_o.contains (Extended_interval_o.create 3 10) 4;;
