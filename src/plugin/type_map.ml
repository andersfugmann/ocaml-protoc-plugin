(** Module to create mapping between proto names 'X.Y.Z' to ocaml names *)
open !StdLabels
open !MoreLabels
open !Utils
open Spec.Descriptor.Google.Protobuf

(* TODO:
   - Map extensions also.
   - Understand proto3_optional flags
   - Better understanding of cyclic definitions.
   - Annotate messages that are maps
*)
let sprintf = Printf.sprintf

type oneof = { name: string; constructor_name: string; type_: string option; }
type field = Plain of { type_: string option }
           | Oneof of oneof list

type entry = { name: string; ocaml_name: string }

(* The map has: proto_type -> ocaml_module * ocaml_name * element_type *)

type element_type =
  | Message of { is_map: bool; fields: (entry * field) list }
  | Enum of entry list
  | Service of entry list


(* The map has: proto_type -> ocaml_module * ocaml_name * element_type *)

type element = {
  module_name: string; (* Module in which the definition resides - derived from the proto name *)
  ocaml_name: string; (* Fully qualified ocaml name of the module; e.g. My_module.Sub_module.X *)
  element_type: element_type;
}

type t = element StringMap.t

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

type scope = { proto_path: string; ocaml_path: string; module_name: string }

let add_scope ~proto_name ~ocaml_name { proto_path; ocaml_path; module_name } =
  let proto_path = sprintf "%s.%s" proto_path proto_name in
  let ocaml_path = match ocaml_path = "" with
    | true -> ocaml_name
    | false -> sprintf "%s.%s" ocaml_path ocaml_name
  in
  { proto_path; ocaml_path; module_name }

let create_name_map: mangle_f:(string -> string) -> f:(?mangle_f:(string -> string) -> string -> string) -> string list -> string StringMap.t = fun ~mangle_f ~f names ->
  Names.create_name_map
    ~standard_f:(f ~mangle_f:(fun x -> x))
    ~mangle_f:(f ~mangle_f)
    names

let element_of_message ~mangle_f ~(scope:scope) fields oneof_decls options =
  let is_map = match options with
    | Some MessageOptions.{ map_entry = Some true; _ } -> true
    | _ -> false
  in

  let plain_fields = List.filter ~f:(fun FieldDescriptorProto.{ oneof_index; _ } -> Option.is_none oneof_index) fields in
  let field_name_map =
    let plain_field_names = List.filter_map ~f:(fun field -> field.FieldDescriptorProto.name) plain_fields in
    let oneof_names = List.filter_map ~f:(fun field -> field.OneofDescriptorProto.name) oneof_decls in
    create_name_map ~mangle_f ~f:Names.field_name (plain_field_names @ oneof_names)
  in
  let oneofs =
    List.mapi ~f:(fun i OneofDescriptorProto.{ name; _ } ->
      let name = Option.value_exn ~message:"Oneof field must have a name" name in
      (* Get all the fields *)
      let oneof_fields =
        List.filter ~f:(function
          | FieldDescriptorProto.{ oneof_index = Some i'; _ } -> i = i'
          | FieldDescriptorProto.{ oneof_index = None; _ } -> false
        ) fields
      in
      let oneof_name_map =
        List.filter_map ~f:(fun field -> field.FieldDescriptorProto.name) oneof_fields
        |> Names.create_name_map
             ~standard_f:(Names.poly_constructor_name ~mangle_f:(fun x -> x))
             ~mangle_f:(Names.poly_constructor_name ~mangle_f)
      in
      let oneofs =
        List.map ~f:(fun FieldDescriptorProto.{ name; type_name; type'; _ } ->
          let name = Option.value_exn ~message:"All fields should have a name" name in
          let type_ = match type' with
            | Some FieldDescriptorProto.Type.TYPE_MESSAGE -> type_name
            | _ -> None
          in
          let constructor_name = StringMap.find name oneof_name_map in
          { name; constructor_name; type_; }
        ) oneof_fields
      in
      let ocaml_name = StringMap.find name field_name_map in
      { name; ocaml_name }, Oneof oneofs
    ) oneof_decls
  in
  let plain_fields =
    List.map ~f:(fun FieldDescriptorProto.{ name; type_name; type'; _ } ->
      let name = Option.value_exn ~message:"All fields should have a name" name in
      let ocaml_name = StringMap.find name field_name_map in
      let type_ = match type' with
        | Some FieldDescriptorProto.Type.TYPE_MESSAGE -> type_name
        | _ -> None
      in
      { name; ocaml_name; }, Plain { type_; }
    ) plain_fields
  in
  let fields = plain_fields @ oneofs in
  {
    module_name = scope.module_name;
    ocaml_name = scope.ocaml_path;
    element_type = Message { is_map; fields };
  }

let element_of_enum ~mangle_f ~scope EnumDescriptorProto.{ value; _ } =
  (* We need the name to come from parent. I.e. though the scope. *)
  let names = List.filter_map ~f:(fun EnumValueDescriptorProto.{ name; _ } -> name) value in
  let name_map = create_name_map ~mangle_f ~f:Names.constructor_name names in
  let constructors =
    List.map ~f:(fun name ->
      { name; ocaml_name = StringMap.find name name_map }
    ) names
  in
  {
    module_name = scope.module_name;
    ocaml_name = scope.ocaml_path;
    element_type = Enum constructors;
  }

let element_of_service ~mangle_f ~scope ServiceDescriptorProto.{ method'; _ } =
  let name_map =
    List.filter_map ~f:(fun MethodDescriptorProto.{ name; _ } -> name) method'
    |> create_name_map ~mangle_f ~f:Names.module_name
  in
  let entries =
    List.map ~f:(fun MethodDescriptorProto.{ name; _ } ->
      let name = Option.value_exn ~message:"Methods must have a name" name in
      let ocaml_name = StringMap.find name name_map in
      { name; ocaml_name }
    ) method'
  in
  {
    module_name = scope.module_name;
    ocaml_name = scope.ocaml_path;
    element_type = Service entries;
  }

let rec traverse_message ~mangle_f ~scope map services DescriptorProto.{ field; nested_type; enum_type; oneof_decl; options; _ } =
  (* Scope contains all messages *)
  let name_map =
    let message_names = List.filter_map ~f:(fun f -> f.DescriptorProto.name) nested_type in
    let enum_names = List.filter_map ~f:(fun e -> e.EnumDescriptorProto.name) enum_type in
    let service_names = List.filter_map ~f:(fun s -> s.ServiceDescriptorProto.name) services in
    create_name_map ~mangle_f ~f:Names.module_name (message_names @ enum_names @ service_names)
  in
  (* Scope contains this element *)
  let message_element = element_of_message ~mangle_f ~scope field oneof_decl options in
  let map = StringMap.add ~key:scope.proto_path ~data:message_element map in

  let map =
    List.fold_left ~init:map ~f:(fun map enum ->
      let proto_name = Option.value_exn ~message:"Enums must have a name" enum.EnumDescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      let element = element_of_enum ~mangle_f ~scope enum in
      StringMap.add ~key:scope.proto_path ~data:element map
    ) enum_type
  in

  let map =
    List.fold_left ~init:map ~f:(fun map message ->
      let proto_name = Option.value_exn ~message:"All messages must have a name" message.DescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      traverse_message ~mangle_f ~scope map [] message
    ) nested_type
  in
  let map =
    List.fold_left ~init:map ~f:(fun map service ->
      let proto_name = Option.value_exn ~message:"All services must have a name" service.ServiceDescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      let element = element_of_service ~mangle_f ~scope service in
      StringMap.add ~key:scope.proto_path ~data:element map
    ) services
  in
  map

let traverse_file ~prefix_module_names map FileDescriptorProto.{ name; message_type = messages; package; enum_type = enums; service = services; extension = extensions; options; _ } =
  let mangle_f = match (Names.has_mangle_option options) with
    | true -> Names.to_snake_case
    | false -> fun x -> x
  in

  let name = Option.value_exn ~message:"All files must have a name" name in
  (* Name is the proto name *)
  let module_name =
    let package = match prefix_module_names with
      | false -> None
      | true -> package
    in
    module_name_of_proto ?package name
  in
  (* Scope should be the fully qualified ocaml name as well as the mapped ocaml name *)
  let scope =
    let proto_path, ocaml_path =
      match package with
      | Some package ->
        let ocaml_path =
          String.split_on_char ~sep:'.' package
          |> List.map ~f:(Names.module_name ~mangle_f)
          |> String.concat ~sep:"."
        in
        "." ^ package, ocaml_path
      | None -> "", ""
    in
    { proto_path; ocaml_path; module_name }
  in

  (* Mimic a message. *)
  let message = DescriptorProto.make ~nested_type:messages ~enum_type:enums ~extension:extensions () in
  let map = traverse_message ~mangle_f map ~scope services message in

  map

(** Construct a set of proto_names (types) that are cyclic *)
let create_cyclic_set map =
  (* Create a map of dependencies for each type.
     This set only notes dependencies for types with only one field or messages that are maps
  *)
  let dependencies =
    StringMap.fold ~init:StringMap.empty ~f:(fun ~key ~data dependencies ->
      let deps =
        match data with
        | { element_type = Message { is_map = true; fields }; _ } ->
          List.filter_map ~f:(function
            | _, Plain { type_ } -> type_
            | _, Oneof _ -> failwith "Oneofs cannot appear in a map"
          ) fields
        | { element_type = Message { is_map = false; fields = [ _, Plain { type_ = Some type_ }] }; _ } -> [ type_ ]
        | { element_type = Message { is_map = false; fields = [ _, Oneof [ { type_ = Some type_; _ } ]] }; _ } -> [ type_ ]
        | _ -> []
      in
      StringMap.add ~key ~data:deps dependencies
    ) map
  in

  (* Determine if a message is cyclic. Its cyclic if its already in the seen set, or if any of its dependencies are cyclic. *)
  let rec is_cyclic seen proto_name =
    (* Lookup the dependencies of proto_name *)
    (* If the proto_name is in the seen set, stop now *)
    match StringSet.mem proto_name seen with
    | true -> true
    | false ->
      let seen = StringSet.add proto_name seen in
      let deps =StringMap.find proto_name dependencies in
      List.exists ~f:(is_cyclic seen) deps
  in
  StringMap.fold ~init:StringSet.empty ~f:(fun ~key ~data:_ cyclic_set ->
    match is_cyclic StringSet.empty key with
    | true -> StringSet.add key cyclic_set
    | false -> cyclic_set
  ) map

let init ~prefix_module_names (files : FileDescriptorProto.t list) =
  let map = List.fold_left ~init:StringMap.empty ~f:(traverse_file ~prefix_module_names) files in
  (* Dump the map, as a test *)
  let cyclic_set = create_cyclic_set map in
  StringMap.iter ~f:(fun ~key ~data:{ module_name; ocaml_name; element_type } ->
    let element_str = match element_type with
      | Message { is_map; fields; _ } ->
        List.map ~f:(function
          | { name; _ }, Plain { type_ } -> sprintf "(%s, %s)" name (Option.value ~default:"<none>" type_)
          | { name; _ }, Oneof _ -> name
        ) fields
        |> String.concat ~sep:"; "
        |> sprintf "is_map: %b, [ %s ]" is_map
      | _ -> ""
    in
    match StringSet.mem key cyclic_set with
    | true -> Printf.eprintf "Cyclic: %s -> %s.%s : %s\n" key module_name ocaml_name element_str
    | false -> ()
  ) map;
  (* StringMap.iter ~f:(fun ~key ~data:{ module_name; ocaml_name; element_type } ->
    match element_type with
    | Message { is_map = true; _ } -> Printf.eprintf "Map: %s -> %s#%s\n" key module_name ocaml_name
    | _ -> ()
     ) map;
  *)

  map


(* We need to create access function for name mappings here *)
