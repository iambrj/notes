open Base
open Stdio

let rec remove_subseq_dups l =
    match l with
    (* note renaming using as *)
    | [] | [_] as l' -> l'
    (* note when guard; compiler loses ability to reason about exhaustiveness
       etc *)
    | h :: (h' :: t) when h = h' -> h :: remove_subseq_dups t
    | h :: t -> h :: remove_subseq_dups t;;

let cuber x = x * x * x;;
