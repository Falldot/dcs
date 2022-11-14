(* Copyright (c) 2022 Falldot. All rights reserved. *)

open Component

let cpp_component_header { name; capacity; _ } =
  Format.sprintf "struct %s { void* f; std::size_t s = 0; std::size_t c = %n;"
    name capacity

let cpp_component_footer ({ name; _ } : component) =
  "void reset() {s = 0;}; ~" ^ name ^ "() {std::free(f);}};"

let cpp_component_constructor ({ name; _ } : component) sizeof =
  name ^ "() { f = std::malloc((" ^ sizeof ^ ") * c); }"

let cpp_component_remove remove =
  "void remove(int i) { " ^ remove ^ " s -= 1; }"

let cpp_component_add args add sizeof =
  Format.sprintf
    "void add(%s) { if (s >= c) { size_t nc = c << 1; f = std::realloc(f, (%s) \
     * nc); c = nc; } %s s += 1; }"
    args sizeof add

let cpp_component_methods (cmp : component) =
  let sizeof fields =
    fst
      (Fields.fold
         (fun _ v (acc, i) ->
           (acc ^ (if i > 0 then "+" else "") ^ "sizeof(" ^ v.typed ^ ")", i + 1))
         fields ("", 0))
  in

  let remove fields =
    let remove' name i =
      match i with
      | 0 ->
          Format.sprintf "%s* f0 = (%s*)f; *(f0 + i) = *(f0 + (s-1));" name name
      | _ ->
          Format.sprintf
            "%s* f%n = (%s*)(f%n + c); *(f%n + i) = *(f%n + (s-1));" name i name
            (i - 1) i i
    in
    fst
      (Fields.fold
         (fun _ v (acc, i) -> (acc ^ remove' v.typed i, i + 1))
         fields ("", 0))
  in

  let add fields =
    let add' k v i =
      match i with
      | 0 -> Format.sprintf "%s* f0 = (%s*)f; *(f0 + s) = %s;" v v k
      | _ ->
          Format.sprintf "%s* f%n = (%s*)(f%n + c); *(f%n + s) = %s;" v i v
            (i - 1) i k
    in
    fst
      (Fields.fold
         (fun k v (acc, i) -> (acc ^ add' k v.typed i, i + 1))
         fields ("", 0))
  in

  let arg fields =
    fst
      (Fields.fold
         (fun k v (acc, i) ->
           ( acc
             ^ (if i > 0 then "," else "")
             ^ v.typed ^ " " ^ k ^ "=" ^ v.default,
             i + 1 ))
         fields ("", 0))
  in

  let s_sizeof = sizeof cmp.fields in
  let s_remove = remove cmp.fields in
  let s_add = add cmp.fields in
  let s_args = arg cmp.fields in

  cpp_component_constructor cmp s_sizeof
  ^ cpp_component_remove s_remove
  ^ cpp_component_add s_args s_add s_sizeof

let cpp_component_generate cmp =
  if Fields.is_empty cmp.fields then ""
  else
    cpp_component_header cmp ^ cpp_component_methods cmp
    ^ cpp_component_footer cmp