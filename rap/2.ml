let c = .<1 + 2>.;;

let rec power n x =
  if n == 0 then 1
  else if n mod 2 == 0
       then square (power (n / 2) x)
       else x * (power (n - 1) x);;

let rec spower n x =
  if n == 0 then .<1>.
  else if n mod 2 == 0
       then .<square .~(spower (n / 2) x)>.
       else .<.~x * .~(spower (n - 1) x)>.;;

let spowern = fun n -> .<fun x -> .~(spower n .<x>.)>.;;
