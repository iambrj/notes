open Core

type etc_services_info =
    {
        service_name : string;
        port         : int;
        protocol     : string;
    }

(* Records support parametric polymorphism! *)
type 'a with_line_num =
    {
        item : 'a;
        line_num : int;
    }

let extract line =
  let open Re in
    let matches =
        let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
        Re.exec (Re.Posix.compile_pat pat) line
    in
    { service_name = Re.Group.get matches 1;
      port = Int.of_string (Re.Group.get matches 2);
      protocol = Re.Group.get matches 3;
    };;

let x = extract "inspider        49150/tcp";;
printf "%s\n" x.service_name;; (* Use dot syntax to access elements *)

(* Irrefutable record pattern matching *)
let extraction_to_string { service_name = name; port = port; protocol = prot } =
    sprintf "%s %i/%s" name port prot;;

(* Field punning *)
let punned_extraction_to_string { service_name ; port ; protocol } =
    sprintf "%s %i/%s" service_name port protocol;;

(* Easy functional updates! *)
let e = extract "iqobject        48619/udp";;
let e1000 = {e with port = 1000};;

type m_etc_services_info =
    {
        service_name : string;
        mutable port : int; (* note mutable keyword usage *)
        protocol     : string;
    }

(* Note that it uses latest record definition when field names are same. To
 * choose, use type annotations *)
let e = {service_name = "foo"; port = 50; protocol = "bar"};;
(* Can mutate as follows *)
e.port <- 25;;
