open! Core

let hello_world_string = [%embed_file_as_string "hello_world.txt"]

let hello_world_string_with_filename =
  [%embed_file_as_string_with_filename "hello_world.txt"]
;;

let hello_world_archived =
  [%embed_file_in_tar_archive_as_string "hello_world.tar.gz:./dir/hello_world.txt"]
;;

let%expect_test "string test" =
  Core.print_string hello_world_string;
  [%expect {xxx| Hello world! |xxx}]
;;

let%expect_test "string with filename test" =
  [%sexp_of: string * string] hello_world_string_with_filename |> Core.print_s;
  [%expect {xxx| (hello_world.txt "Hello world!\n") |xxx}]
;;

let%expect_test "tar test" =
  Core.print_string hello_world_archived;
  [%expect {xxx| Hello world! |xxx}]
;;
