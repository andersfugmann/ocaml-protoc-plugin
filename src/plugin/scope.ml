open StdLabels
open MoreLabels

open Utils

let dump_ocaml_names = false
let dump_type_tree = false


(* Extend functionality such that under import_module_name the module names is prefixed with package names *)
(* i.e. prefix module name *)
(* And also allow exposure to the module names to create correct mappings *)
(* module_name must be prefixed *)

(** Module to avoid name clashes in a local scope *)
module Local = struct
  type t = (string, unit) Hashtbl.t
  let init () : t = Hashtbl.create 2
  let get_unique_name t preferred_name =
    let rec inner name =
      match Hashtbl.mem t name with
      | true -> inner (name ^ "'")
      | false when Names.is_reserved name -> inner (name ^ "'")
      | false -> name

   in
   let name = inner preferred_name in
   Hashtbl.add t ~key:name ~data:();
   name
end

open Spec.Descriptor.Google.Protobuf

let import_module_name = "Imported'modules"
let this_module_alias = "This'_"

type t = { module_name: string;
           package_depth: int;
           proto_path: string list;
           type_db: Type_tree.element StringMap.t;
           ocaml_names: StringSet.t;
           file_names: string StringMap.t; (** proto file -> ocaml module name *)
         }

let init ~params files =
  let file_names, type_db = Type_tree.create_db ~prefix_module_names:params.Parameters.prefix_output_with_package files in
  let ocaml_names =
    StringMap.fold ~init:StringSet.empty
      ~f:(fun ~key:_ ~data:Type_tree.{ocaml_name; _} acc ->
      StringSet.add ocaml_name acc
    ) type_db
  in
  if dump_ocaml_names then
    StringSet.iter ~f:(Printf.eprintf "%s\n") ocaml_names;
  if dump_type_tree then
    StringMap.iter ~f:(fun ~key ~data:Type_tree.{module_name; ocaml_name; _} -> Printf.eprintf "%s: %s - %s\n" key module_name ocaml_name) type_db;

  { module_name = ""; proto_path = []; package_depth = 0; type_db; ocaml_names; file_names }

let for_descriptor ~params t FileDescriptorProto.{ name; package; _ } =
  let name = Option.value_exn ~message:"All file descriptors must have a name" name in
  let module_name =
    let package = match params.Parameters.prefix_output_with_package with
      | true -> package
      | false -> None
    in
    Type_tree.module_name_of_proto ?package name
  in
  let package_depth = Option.value_map ~default:0 ~f:(fun p -> String.split_on_char ~sep:'.' p |> List.length) package in
  { t with package_depth; module_name; proto_path = [] }

let get_proto_path t =
  "" :: (List.rev t.proto_path) |> String.concat ~sep:"."

let push: t -> string -> t = fun t name -> { t with proto_path = name :: t.proto_path }

let get_scoped_name ?postfix t name =
  (* Take the first n elements from the list *)
  let take n l =
    let rec inner = function
      | (0, _) -> []
      | (_, []) -> []
      | (n, x :: xs) -> x :: inner (n - 1, xs)
    in
    inner (n, l)
  in

  let name = Option.value_exn ~message:"Does not contain a name" name in
  let Type_tree.{ ocaml_name; module_name; _ } = StringMap.find name t.type_db in

  let rec resolve path_rev module_name =
    let path = "" :: List.rev_append path_rev module_name |> String.concat ~sep:"." in
    match StringMap.mem path t.type_db, path_rev with
    | true, _ ->  Some path
    | false, [] -> None
    | false, _ :: ps -> resolve ps module_name
  in
  let search name =
    let paths = String.split_on_char ~sep:'.' name |> List.rev in
    let rec inner path paths =
      let p = resolve t.proto_path path in
      match p, paths with
      | Some path', _ when path' = name ->
        String.split_on_char ~sep:'.' ocaml_name
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
    | true -> search name
    | false -> Printf.sprintf "%s.%s.%s" import_module_name module_name ocaml_name |> Option.some
  in
  match postfix, type_name with
  | Some postfix, Some "" -> postfix
  | None, Some "" -> this_module_alias
  | None, Some type_name -> type_name
  | Some postfix, Some type_name -> Printf.sprintf "%s.%s" type_name postfix
  | _, None -> failwith_f "Unable to reference '%s'. This is due to a limitation in the Ocaml mappings. To work around this limitation make sure to use a unique package name" name

let get_name t name =
  let path = Printf.sprintf "%s.%s" (get_proto_path t) name in
  match StringMap.find_opt path t.type_db with
    | Some { ocaml_name; _ } -> String.split_on_char ~sep:'.' ocaml_name |> List.rev |> List.hd
    | None -> failwith_f "Cannot find '%s' in '%s'." name (get_proto_path t)

let get_name_exn t name =
  let name = Option.value_exn ~message:"Does not contain a name" name in
  get_name t name

let get_package_name t =
  match List.tl t.proto_path with
  | [] -> None
  | xs -> List.rev xs |> String.concat ~sep:"." |> Option.some

let get_module_name ~filename t =
  StringMap.find_opt filename t.file_names
  |> Option.value_exn ~message:(Printf.sprintf "Could not find proto file '%s'" filename)

let is_cyclic t =
  let Type_tree.{ cyclic; _ } = StringMap.find (get_proto_path t) t.type_db in
  cyclic
