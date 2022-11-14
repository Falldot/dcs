(* Copyright (c) 2022 Falldot. All rights reserved. *)

type effect_flag = Required | Effect
type overall_flag = Personal | Overall

type field = {
  typed : string;
  default : string;
  effect : effect_flag;
  overall : overall_flag;
}

module Fields = Map.Make (String)

type declared_flag = Declered | Undeclered

type component = {
  name : string;
  capacity : int;
  fields : field Fields.t;
  childrens : string list;
  declared : declared_flag;
}

type base_type = string
type token = Component of component | Base of base_type

(* let map f c =
     match c with
     | Declered v -> Declered (f v)
     | Undeclered v -> Undeclered (f v)
     | Base v -> Base v

   let merge f cf ct =
     let merge' ct' = map (fun v -> f v ct') cf in
     match ct with
     | Declered v -> merge' v
     | Undeclered v -> merge' v
     | Base v -> Base v

   let fold f c =
     match c with Declered v -> f v | Undeclered v -> f v | Base v -> Base v *)

type component_error_data = { name : string; error : string }
