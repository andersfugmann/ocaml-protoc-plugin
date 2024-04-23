open !StdLabels
open !MoreLabels
open !Utils

type char_type = Lower | Upper | Neither

(** Taken from: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let is_reserved = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do" | "done"
  | "downto" | "else" | "end" | "exception" | "external" | "false" | "for" | "fun"
  | "function" | "functor" | "if" | "in" | "include" | "inherit" | "initializer"
  | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor" | "match" | "method"
  | "mod" | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or"
  | "private" | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try" | "type"
  | "val" | "virtual" | "when" | "while" | "with" -> true
  | _ -> false

let to_snake_case ident =
  let to_list s =
    let r = ref [] in
    String.iter ~f:(fun c -> r := c :: !r) s;
    List.rev !r
  in
  let to_string l =
    let bytes = Bytes.create (List.length l) in
    List.iteri ~f:(fun i c -> Bytes.set bytes i c) l;
    Bytes.to_string bytes
  in
  let char_case = function
    | 'a' .. 'z' -> Lower
    | 'A' .. 'Z' -> Upper
    | _ -> Neither
  in
  let is_lower c = char_case c = Lower in
  let is_upper c = char_case c = Upper in

  let rec to_snake_case = function
    | c1 :: c2 :: cs when is_lower c1 && is_upper c2 ->
      c1 :: '_' :: c2 :: to_snake_case cs
    | c1 :: cs ->
      c1 :: (to_snake_case cs)
    | [] -> []
  in
  to_list ident
  |> to_snake_case
  |> to_string
  |> String.lowercase_ascii
  |> String.capitalize_ascii

let field_name field_name =
  String.uncapitalize_ascii field_name

let method_name = field_name

let module_name name =
  match name.[0] with
  | '_' -> "P" ^ name
  | _ -> String.capitalize_ascii name

let module_name_of_proto ?package proto_file =
  Filename.chop_extension proto_file
  |> Filename.basename
  |> (
    match package with
    | Some package -> Printf.sprintf "%s_%s" package
    | None -> fun s -> s
  )
  |> String.capitalize_ascii
  |> String.map ~f:(function '-' | '.' -> '_' | c -> c)

let constructor_name = module_name

let poly_constructor_name name =
  "`" ^ String.capitalize_ascii name

let has_mangle_option options =
  match options with
  | None -> false
  | Some options ->
    Spec.Options.Ocaml_options.get options
    |> Ocaml_protoc_plugin.Result.get ~msg:"Could not parse ocaml-protoc-plugin option id 1074"
    |> function
    | Some v -> v
    | None -> false

(** Map a set of proto_names to ocaml names. The mapping will be uniq and prioritize minimum differences
    [mangle_f] is a generic mangle function
    [name_f] is a function to convert a name into the ocaml required type (E.g. capitalize or adding a '`')
*)
let create_ocaml_mapping: ?name_map:string StringMap.t -> ?mangle_f:(string -> string) -> name_f:(string -> string) -> string list -> string StringMap.t =
  fun ?(name_map=StringMap.empty) ?(mangle_f=(fun x -> x)) ~name_f proto_names ->

  (* Expand names into proto_name, mapped_name, mangled_name *)
  let expanded_names =
    let name_f proto_name =
      name_f proto_name |> function
      | name when is_reserved name -> name ^ "'"
      | name -> name
    in
    let mangle_f proto_name = name_f (mangle_f proto_name) in

    List.map ~f:(fun proto_name ->
      let standard_name = name_f proto_name in
      let mangled_name = mangle_f proto_name in
      (proto_name, mangled_name, standard_name)
    ) proto_names
  in

  (* Sort the names to create a stable-like mapping *)
  let expanded_names =
    let l = String.lowercase_ascii in
    let cmp n1 n2 =
      match n1, n2 with
      (* Ocaml name = proto name *)
      | ( proto_name,  mangled_name, _standard_name), ( proto_name',  mangled_name', _standard_name') when
          proto_name = mangled_name && proto_name' = mangled_name' -> 0
      | ( proto_name,  mangled_name, _standard_name), (_proto_name', _mangled_name', _standard_name') when
          proto_name = mangled_name -> -1
      | (_proto_name, _mangled_name, _standard_name), ( proto_name', mangled_name', _standard_name') when
          proto_name' = mangled_name' -> 1
      (* Ocaml name = standard name *)
      | (_proto_name,  mangled_name,  standard_name), (_proto_name',  mangled_name',  standard_name') when
          mangled_name = standard_name && mangled_name' = standard_name'-> 0
      | (_proto_name,  mangled_name,  standard_name), (_proto_name', _mangled_name', _standard_name') when
          mangled_name = standard_name -> -1
      | (_proto_name, _mangled_name, _standard_name), (_proto_name',  mangled_name',  standard_name') when
          mangled_name' = standard_name'-> 1
      (* Lower case ocaml name = lower case proto name *)
      | ( proto_name,  mangled_name, _standard_name), ( proto_name',  mangled_name', _standard_name') when
          l proto_name = l mangled_name && l proto_name' = l mangled_name' -> 0
      | ( proto_name,  mangled_name, _standard_name), (_proto_name', _mangled_name', _standard_name') when
          l proto_name = l mangled_name -> -1
      | (_proto_name, _mangled_name, _standard_name), ( proto_name', mangled_name', _standard_name') when
          l proto_name' = l mangled_name' -> 1
      (* Lower case Ocaml name = lower case standard name *)
      | (_proto_name,  mangled_name,  standard_name), (_proto_name',  mangled_name',  standard_name') when
          l mangled_name = l standard_name && l mangled_name' = l standard_name'-> 0
      | (_proto_name,  mangled_name,  standard_name), (_proto_name', _mangled_name', _standard_name') when
          l mangled_name = l standard_name -> -1
      | (_proto_name, _mangled_name, _standard_name), (_proto_name',  mangled_name',  standard_name') when
          l mangled_name' = l standard_name'-> 1
      (* No mapping available. *)
      | ( _proto_name, _mangled_name, _standard_name), ( _proto_name', _mangled_name', _standard_name') -> 0
    in
    (* Stable sort is important here *)
    List.stable_sort ~cmp expanded_names
  in

  let seen =
    StringMap.fold ~init:StringSet.empty ~f:(fun ~key:_ ~data:ocaml_name seen ->
      StringSet.add ocaml_name seen
    ) name_map
  in
  let name_map, _seen =
    let rec make_uniq seen name =
      match StringSet.mem name seen with
      | false -> name
      | true -> make_uniq seen (name ^ "'")
    in
    List.fold_left ~init:(name_map, seen) ~f:(
      fun (map, seen) (proto_name, ocaml_name, _) ->
        let ocaml_name = make_uniq seen ocaml_name in
        StringMap.add ~key:proto_name ~data:ocaml_name map,
        StringSet.add ocaml_name seen
    ) expanded_names
  in
  name_map
