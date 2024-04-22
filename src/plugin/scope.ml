open !StdLabels
open !MoreLabels
open !Utils

let import_module_name = "Imported'modules"
let this_module_alias = "This'_"

type t = { module_name: string;
           proto_path: string list;
         }

let init ~module_name =
  { module_name; proto_path = []; }

let get_proto_path ?name t =
  let proto_path = match name with
    | Some name -> name :: t.proto_path
    | None -> t.proto_path
  in
  "" :: (List.rev proto_path) |> String.concat ~sep:"."

let push: t -> string -> t = fun t name -> { t with proto_path = name :: t.proto_path }

(** Change to work over proto_names. We can then map back once we know how to reference
    (As we know the target name)
*)
let get_scoped_name_type_db ?postfix t type_db proto_path =
  (* Take the first n elements from the list *)
  let take n l =
    let rec inner = function
      | (0, _) -> []
      | (_, []) -> []
      | (n, x :: xs) -> x :: inner (n - 1, xs)
    in
    inner (n, l)
  in

  let proto_path = Option.value_exn ~message:"No name given" proto_path in
  let ocaml_path = Type_db.get_ocaml_path type_db proto_path in
  let module_name = Type_db.get_location type_db proto_path in

  (* given a relative path, return the proto_path that it resolves to in the given scope *)
  let rec resolve path_rev scope =
    let path = "" :: List.rev_append path_rev scope |> String.concat ~sep:"." in
    match Type_db.exists type_db path, path_rev with
    | true, _ ->  Some path
    | false, [] -> None
    | false, _ :: ps -> resolve ps scope
  in
  (* Find the relative name for the given proto_path is possible *)
  let search proto_type =
    let paths = String.split_on_char ~sep:'.' proto_type |> List.rev in
    let rec inner path paths =
      let p = resolve t.proto_path path in
      match p, paths with
      | Some path', _ when path' = proto_path ->
        (* Found. Return the Ocaml name for the relative path *)
        String.split_on_char ~sep:'.' ocaml_path
        |> List.rev
        |> take (List.length path)
        |> List.rev
        |> String.concat ~sep:"."
        |> Option.some
      | _, p :: paths -> inner (p :: path) paths
      | _, [] -> None
    in
    inner [] paths
  in
  let type_name =
    match t.module_name = module_name with
    | true -> search proto_path
    | false -> Printf.sprintf "%s.%s.%s" import_module_name module_name ocaml_path |> Option.some
  in
  match postfix, type_name with
  | Some postfix, Some "" -> postfix
  | None, Some "" -> this_module_alias
  | None, Some type_name -> type_name
  | Some postfix, Some type_name -> Printf.sprintf "%s.%s" type_name postfix
  | _, None -> failwith_f "Unable to reference '%s'. This is due to a limitation in the Ocaml mappings. To work around this limitation make sure to use a unique package name" proto_path
