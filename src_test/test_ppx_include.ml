
let () =
  let open Ppx_types in
  let loc = ("main.ml", (4, 4), (0, 0)) in
  print_endline (show_loc loc);

