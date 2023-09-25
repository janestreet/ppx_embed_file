open Core
open Ppxlib
open Ast_builder.Default

let file_path_to_absolute_string ~loc compile_time_file_path =
  let open (val Ast_builder.make loc) in
  let entire_file = In_channel.with_file compile_time_file_path ~f:In_channel.input_all in
  estring entire_file
;;

let file_path_to_absolute_string_with_filename ~loc compile_time_file_path =
  let entire_file = In_channel.with_file compile_time_file_path ~f:In_channel.input_all in
  [%expr
    [%e estring ~loc:{ loc with loc_ghost = true } compile_time_file_path]
    , [%e estring ~loc entire_file]]
;;

let decompress_tar_file ~loc archive filename =
  let tempdir = Core_unix.mkdtemp "./ppx-embed-tmp" in
  let () = Shell.run "tar" [ "-C"; tempdir; "-xf"; archive; filename ] in
  let result = file_path_to_absolute_string ~loc (sprintf "%s/%s" tempdir filename) in
  let () = Shell.run "rm" [ "-rf"; tempdir ] in
  result
;;

let embed_file_as_string =
  Extension.V3.declare
    "embed_file_as_string"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt relative_file_path ->
      file_path_to_absolute_string
        ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
        relative_file_path)
  |> Ppxlib.Context_free.Rule.extension
;;

let embed_file_as_string_with_filename =
  Extension.V3.declare
    "embed_file_as_string_with_filename"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt relative_file_path ->
      file_path_to_absolute_string_with_filename
        ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
        relative_file_path)
  |> Ppxlib.Context_free.Rule.extension
;;

let embed_file_in_tar_archive_as_string =
  Extension.V3.declare
    "embed_file_in_tar_archive_as_string"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt archive_and_file_path ->
      let archive_and_file_path = String.split ~on:':' archive_and_file_path in
      match archive_and_file_path with
      | [ archive; file_path ] ->
        decompress_tar_file
          ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
          archive
          file_path
      | _ ->
        raise_s
          [%message "Archive file paths should be in the form archive_path:file_path"])
  |> Ppxlib.Context_free.Rule.extension
;;

let () =
  Driver.register_transformation
    ~rules:
      [ embed_file_as_string
      ; embed_file_as_string_with_filename
      ; embed_file_in_tar_archive_as_string
      ]
    "ppx_embed_file"
;;
