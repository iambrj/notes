let perf : (unit -> a) -> a = fun th ->
  let start_time = Sys.time() in
  let r = th () in
  let elapsed_time = Sys.time() - start_time in
  Printf.printf "\nit took %g secs\n" elapsed_time;
  r
