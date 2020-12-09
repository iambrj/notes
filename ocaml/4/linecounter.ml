open Base
open Stdio

let build_counts = fun () ->
    In_channel.fold_lines In_channel.stdin ~init:Hidden_core.empty ~f:Hidden_core.touch
    (* module names are capitalized even if filenames aren't! *)

let () =
    build_counts ()
        |> Hidden_core.to_list
        |> List.sort ~compare:(fun (_,x) (_,y) -> Int.descending x y)
        |> (fun l -> List.take l 10)
        |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line);;
