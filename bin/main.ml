(* Copyright (c) 2022 Falldot. All rights reserved. *)

open Context
open Component

let create_file_context name text =
  if not (Sys.file_exists "cpp" && Sys.is_directory "cpp") then
    Sys.mkdir "cpp" 0o755;
  let file = open_out ("./cpp/" ^ name ^ ".h") in
  Printf.fprintf file "%s" text;
  close_out file

let print_context ctx =
  Context.fold
    (fun _ v acc -> acc ^ Generator.cpp_component_generate v)
    ctx "#include <cstdlib>\n"
  |> create_file_context "test"

let print_errors =
  List.iter (fun v -> Format.printf "\n%s: %s\n%!" v.name v.error)

let main ctx =
  Validator.make [] ctx |> add_base_type "int" |> add_base_type "char"
  |> add_base_type "float" |> add_base_type "double"
  |> add_component "Position" 4
  |> add_field "Position" "x" "int" "0" Required Personal
  |> add_field "Position" "y" "int" "0" Required Personal
  |> add_component "Direction" 4
  |> add_field "Direction" "x" "char" "0" Required Personal
  |> add_field "Direction" "y" "double" "0" Required Personal
  |> add_component "Speed" 4
  |> add_field "Speed" "value" "float" "0" Required Personal
  |> add_component "Transform" 4
  |> add_field "Transform" "Position" "Position" "0" Required Personal
  |> add_field "Transform" "Direction" "Direction" "0" Required Personal
  |> add_field "Transform" "Speed" "Speed" "0" Required Personal
  |> validate_context
  |> is_errors print_errors print_context
;;

create_file_context "huypola"

let _ = main Context.empty
