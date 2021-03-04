open Stdio
open Base

let binary_search arr x =
    let length = Array.length arr in
    let l = ref 0 in
    let u = ref length in
    let mid = ref ((!l + !u) / 2) in
    while !l < !u && !mid < !u do
        (mid := ((!l + !u) / 2))
        |> fun () ->
            if (arr.(!mid) = x)
            then !mid
            else if x < arr.(!mid)
                 then ((u := (!mid - 1)) |> (fun () -> -1))
                 else ((l := (!mid + 1)) |> (fun () -> -1))
    done ;;
