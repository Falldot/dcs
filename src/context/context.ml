(* Copyright (c) 2022 Falldot. All rights reserved. *)

open Component
module Context = Map.Make (String)

let create_declered_error name =
  {
    name = "Syntax error";
    error = "Компонент '" ^ name ^ "' уже был объявлен!";
  }

let create_undeclered_error name =
  { name = "Syntax error"; error = "Компонент '" ^ name ^ "' не был объявлен!" }

let create_declered_base_error name =
  {
    name = "Syntax error";
    error = "Нельзя переопределить базовый тип '" ^ name ^ "'!";
  }

let create_add_field_base_error name =
  {
    name = "Syntax error";
    error = "Тип '" ^ name ^ "' не являеться компонентом!";
  }

let create_recursion_error to_name name from_name =
  {
    name = "Syntax error";
    error =
      "Рекурсивный тип '" ^ from_name ^ "' как поле '" ^ name
      ^ "' в компоненте '" ^ to_name ^ "' недопустим!";
  }

let declare_component name capacity =
  Component
    {
      name;
      capacity;
      fields = Fields.empty;
      declared = Declered;
      childrens = [];
    }

let undeclare_component name =
  Component
    {
      name;
      capacity = 0;
      fields = Fields.empty;
      declared = Undeclered;
      childrens = [];
    }

let take_component name ctx =
  match Context.find_opt name ctx with
  (* Берем компонент *)
  | Some c -> c
  (* Если не нет, создаем, но он еще не был объявлен как следует *)
  | None -> undeclare_component name

let add_base_type name =
  Validator.chain (fun errs ctx -> (errs, Context.add name (Base name) ctx))

let add_component name capacity =
  Validator.chain (fun errs ctx ->
      match Context.find_opt name ctx with
      (* Объявляем компонент *)
      | None -> (errs, Context.add name (declare_component name capacity) ctx)
      (* Нельзя переопределять базовые типы *)
      | Some (Base _) -> (create_declered_base_error name :: errs, ctx)
      | Some (Component cmp) -> (
          match cmp.declared with
          (* Создаем ошибку, т.к. компонент уже был объявлен *)
          | Declered -> (create_declered_error name :: errs, ctx)
          (* Компонент был не явно объявлен, объявляем явно и даем наследникам поля *)
          | Undeclered ->
              ( errs,
                Context.add name
                  (Component
                     {
                       cmp with
                       capacity;
                       childrens = List.map (fun v -> v) cmp.childrens;
                     })
                  ctx )))

let add_field to_name name from_name default effect overall =
  Validator.chain (fun errs ctx ->
      let t_to = take_component to_name ctx in
      let t_from = take_component from_name ctx in

      match t_to with
      (* Ей, нельзя добавлять поля базовому типу! *)
      | Base t_to_name -> (create_add_field_base_error t_to_name :: errs, ctx)
      | Component t_to_cmp -> (
          ( errs,
            match t_from with
            | Base t_to_name ->
                Context.add to_name
                  (Component
                     {
                       t_to_cmp with
                       fields =
                         Fields.add name
                           { typed = t_to_name; default; effect; overall }
                           t_to_cmp.fields;
                     })
                  ctx
            | Component t_from_cmp ->
                ctx
                |> Context.add to_name
                     (Component
                        {
                          t_to_cmp with
                          fields =
                            Fields.fold
                              (fun k v acc ->
                                Fields.add (name ^ k)
                                  { v with default; effect; overall }
                                  acc)
                              t_from_cmp.fields t_to_cmp.fields;
                        })
                |> Context.add from_name
                     (Component
                        {
                          t_from_cmp with
                          childrens = to_name :: t_from_cmp.childrens;
                        }) )))

let is_errors f g ctx = match ctx with [], cmps -> g cmps | es, _ -> f es

let validate_context ctx =
  let check_on_undeclered name t (l, r) =
    match t with
    | Base _ -> (l, r)
    | Component cmp -> (
        match cmp.declared with
        | Declered -> (l, Context.add name cmp r)
        | Undeclered -> (create_undeclered_error name :: l, r))
  in

  let validate_context' l r =
    Context.fold check_on_undeclered r (l, Context.empty)
  in

  Validator.chain validate_context' ctx