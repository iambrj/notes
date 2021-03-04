open Base

let apply f_opt x =
    match f_opt with
    | None -> None
    | Some f -> Some (f x)

include Option
