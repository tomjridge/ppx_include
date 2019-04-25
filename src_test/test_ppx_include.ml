
let () =
  let open Ppx_types in
  let open C in
  let loc = ("main.ml", (4, 4), (0, 0)) in
  ignore (show_loc loc);
  let s = B.get_s in
  ignore s;
  ignore B.b

