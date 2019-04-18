(* This is a large recursive type, included in both interface and impl file *)
type loc = string * (int * int) * (int * int) [@@deriving show]

(** Some Documentation *)
type id = string * loc [@@deriving show]

(** More Docs *)
and decl =
  | Decl_Func of decl_func (** Some more docs *)
[@@deriving show]

(* A normal comment *)
and decl_func = {
  df_Name: id;
} [@@deriving show]
