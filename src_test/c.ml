(* Comments represent the code included *)

module rec A : sig

  (* type t = string *)
  (* val s: t *)

  [%%include "a.mli.inc"]
  [%%include "a1.mli.inc"]

  val a: int

end = struct

  (* type t = string *)
  (* let s = "some string" *)

  [%%include "a.ml.inc"]
  [%%include "a1.ml.inc"]

  let a = 42

end

and B : sig

  (* val get_s: A.t *)

  [%%include "b.mli.inc"]

  val b: int

end = struct

  (* let get_s = *)
  (*   A.s *)

  [%%include "b.ml.inc"]

  let b = A.a

end
