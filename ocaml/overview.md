Variable identifiers should start with lowercase letter or underscore
Use let binding to assign values
    let s = 2;;
Use let binding to define functions
    let ratio x y = Float.of_int /. Float.of_int y;;
Module names always start with a capital letter
Create tuple by joining values together with a comma
    let a_tuple = (3, "three");;
Use pattern-matching syntax to extract components
	let (x,y) = a_tuple;;
Use ** operator for raising a floating-point number to a power
    let distance (x1,y1) (x2,y2) = sqrt((xi -. x2) ** 2. +. (y1 - y2) ** 2.);;
Use square braces and semicolons to create lists
    let languages = ["OCaml";"Perl";"C"];;
Use :: for adding elements to the front of a list
    "French" :: "Spanish" :: langauges;;
Use @ to concatenate two lists
    [1;2;3] @ [4;5;6]
Use match to extract values form lists
    let my_favourite_langauge languages = 
        match langauges with
        | first :: the_rest -> first
        | [] -> "OCaml" (* A good default *)
Define recursive functions using rec keyword and match
    let rec sum l =
        match l with
        | [] -> 0 (* base case *)
        | hd :: tl -> hd + sum tl (* inductive case *)
        ;;
Use None and Some to create options - lists with either 1 or 0 elements
    let divide x y =
        if y = 0 then None else Some (x/y);;
Some and None are constructors that build optional values, just as :: and []
build lists
Use pattern matching to examine contents of an option
    let log_entry maybe_time message = 
        let time = 
            match maybe_time with
            | Some x -> x
            | None -> Time.now()
        in
        Time.to_sec_string time ^ " -- " ^ message
;;
Use in to scope let bindings upto double semicolon
Use {} to define record datatype
    let point2d = { x : float; y : float };;
    let p = { x = 3.; y = -4.};;
Use pattern matching or dot notation to extract values from records
    let magnitude { x = x_pos; y = y_pos} = sqrt (x_pos ** 2. +. y_pos ** 2.);;
	let distance v1 v2 = magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;
Use variants to combine objects of multiple types together
    type circle_desc = { center: point2d; radius: float }
    type rect_desc = { lower_left: pont2d; width: float; height: float }
    type segment_desc = { endpoint1: point2d; endpoint2: point2d };;
    type scene_element = 
        | Circle of circle_desc
        | Rect of rect_desc
        | Segment of segment_desc
    ;;
| character separates different cases of the variant
Use case structure to deal with individual datatypes of a variant
Use [||] to create arrays, .() to access and <- to modify
    let numbers = [| 1; 2; 3; 4 |];;
    numbers.(2) <- 4;; (* modification *)
Use mutable keyword to make records mutable
    type running_sum = 
    {
        mutable sum: float;
        mutable sum_sq: float;
        mutabel samples: int;
    }
For syntax
    let ar = Array.init 20 ~f:(fun i->i);;
    premute arr;;
While syntax
    let pos = ref 0 in
    while !pos < Array.length && array.(!pos) >= 0 do
        pos := !pos + 1
    done;
    if !pos = Array.length array then None else Some !pos
    ;;
