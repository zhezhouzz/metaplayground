open Language_utils
open Ocaml5_parser
open Convention
open Msyntax
open Yojson.Basic.Util

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys_unix.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let load_config meta_fname =
  let metaj = Yojson.Basic.from_file meta_fname in
  let l = metaj |> member "global" |> to_list in
  let l =
    List.map
      (fun j ->
        let name = j |> member "name" |> to_string in
        match j |> member "kind" |> to_string with
        | "typed" -> PreDefTyped name
        | "plain" -> PreDefPlain name
        | _ -> failwith "wrong meta config")
      l
  in
  mk_predefines l

let dune_prefix = "\n(library\n (name syntax)\n (wrapped false)\n (modules"

let dune_postfix =
  "\n\
  \ )\n\
  \ (preprocess (pps ppx_optcomp ppx_compare ppx_sexp_conv))\n\
  \ (libraries language_utils)\n\
   )"

let mk_ocamlfile config_file dir filename () =
  let filename = filename ^ ".ml" in
  let res =
    handle_type_declaration (load_config config_file) (dir ^ "/_" ^ filename)
  in
  let oc = open_out (dir ^ "/" ^ "/__" ^ filename) in
  Printf.fprintf oc "%s\n" res;
  close_out oc

let cmd_config_source summary f =
  Command.basic ~summary
    Command.Let_syntax.(
      let%map_open config_file = anon ("config_file" %: regular_file)
      and dir = anon ("source_code_dir" %: string)
      and filename = anon ("file name" %: string) in
      f config_file dir filename)

let cmds = [ ("gen-syntax-file", cmd_config_source "print rty" mk_ocamlfile) ]
