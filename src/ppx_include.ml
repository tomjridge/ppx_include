open Ast_403

open Asttypes
open Parsetree
open Ast_mapper

let ocaml_version = Migrate_parsetree.Versions.ocaml_403
let from_current = Migrate_parsetree.(Versions.migrate Versions.ocaml_current ocaml_version)

let raise_errorf ?sub ?if_highlight ?loc message =
  message
  |> Printf.kprintf (fun str ->
      let err = Location.error ?sub ?if_highlight ?loc str in
      raise (Location.Error err))

let filename_of_payload ~loc payload =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (file, None)); _ }, _); _ }] ->
    file
  | _ ->
    raise_errorf ~loc "[%%include]: invalid syntax"

let lexbuf_of_payload ~load_path ~loc payload =
  let filename = filename_of_payload ~loc payload in
  let load_paths =
    (Filename.dirname loc.Location.loc_start.Lexing.pos_fname :: load_path) |>
    List.map (fun dir -> Filename.concat dir filename)
  in
  try
    let open Lexing in
    let lexbuf = load_paths |>
                 List.find (fun intf -> Sys.file_exists intf) |>
                 open_in |>
                 Lexing.from_channel in
    let start_p = { lexbuf.lex_start_p with pos_fname = filename } in
    let curr_p = { lexbuf.lex_curr_p with pos_fname = filename } in
    lexbuf.lex_start_p <- start_p;
    lexbuf.lex_curr_p <- curr_p;
    lexbuf
  with Not_found ->
    raise_errorf ~loc "[%%include]: cannot locate file %S" filename

let rec structure ~load_path mapper items =
  match items with
  | { pstr_desc = Pstr_extension (({ txt = "include"; loc }, payload), _); _ } :: items ->
    let old_struct = (Parse.implementation (lexbuf_of_payload ~load_path ~loc payload)) in
    let cur_struct = from_current.copy_structure old_struct in
    let item = mapper.structure mapper cur_struct in
    structure ~load_path mapper item @ structure ~load_path mapper items
  | item :: items ->
    mapper.structure_item mapper item :: structure ~load_path mapper items
  | [] -> []

let rec signature ~load_path mapper items =
  match items with
  | { psig_desc = Psig_extension (({ txt = "include"; loc }, payload), _); _ } :: items ->
    let old_interface = (Parse.interface (lexbuf_of_payload ~load_path ~loc payload)) in
    let cur_interface = from_current.copy_signature old_interface in
    let item = mapper.signature mapper cur_interface in
    signature ~load_path mapper item @ signature ~load_path mapper items
  | item :: items ->
    mapper.signature_item mapper item :: signature mapper ~load_path items
  | [] -> []

let mapper ~load_path _config _cookies =
  let structure = structure ~load_path in
  let signature = signature ~load_path in
  { default_mapper with structure; signature; }

let () =
  let open Migrate_parsetree in
  Driver.register
    ~name:"ppx_include"
    ~args:[]
    ~position:(-10000)
    ocaml_version
    (fun config cookies -> mapper ~load_path:config.load_path config cookies)
