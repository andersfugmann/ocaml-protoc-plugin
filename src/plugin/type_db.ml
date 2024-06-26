(** Module to create mapping between proto names 'X.Y.Z' to ocaml names *)

open !StdLabels
open !MoreLabels
open !Utils
open Spec.Descriptor.Google.Protobuf


let sprintf = Printf.sprintf

type oneof = { name: string; constructor_name: string; type_: string option; }
type field = Plain of { type_: string option }
           | Oneof of oneof list

type entry = { name: string; ocaml_name: string }

(* The map has: proto_type -> ocaml_module * ocaml_name * element_type *)

type element_type =
  | Message of { map_type: DescriptorProto.t option; fields: (entry * field) list }
  | Enum of entry list
  | Service of entry list
  | Package
  | Extension

(** Name of extensions field *)
let extensions_name = "extensions'"

let string_of_element_type = function
  | Message _ -> "Message"
  | Enum _ -> "Enum"
  | Service _ -> "Service"
  | Package -> "Package"
  | Extension -> "Extension"

(* The map has: proto_type -> ocaml_module * ocaml_name * element_type *)

type scope = { proto_path: string; ocaml_path: string; ocaml_name: string; module_name: string }

type t = { map: (scope * element_type) StringMap.t; cyclic_set: StringSet.t; file_map: string StringMap.t }

let add_scope ~proto_name ~ocaml_name { proto_path; ocaml_path; module_name; ocaml_name = prev; } =
  let proto_path = sprintf "%s.%s" proto_path proto_name in
  let ocaml_path = match ocaml_path = "" with
    | true -> prev
    | false -> sprintf "%s.%s" ocaml_path prev
  in
  { proto_path; ocaml_path; ocaml_name; module_name }

let element_of_message ~params ~mangle_f descriptorproto =
  let DescriptorProto.{ field = fields; oneof_decl = oneof_decls; options; extension_range; _ } = descriptorproto in
  let map_type = match options with
    | Some MessageOptions.{ map_entry = Some true; _ } -> Some descriptorproto
    | _ -> None
  in

  (* Create a set of oneof index' that should be emitted *)
  let _, oneofs =
    List.fold_left ~init:(0, IntSet.empty) ~f:(fun (index, acc) _oneof_decl ->
      let count =
        List.fold_left ~init:0 ~f:(fun acc -> function
          | FieldDescriptorProto.{ proto3_optional = Some true; _ } -> acc
          | FieldDescriptorProto.{ oneof_index = Some oneof_index; _ } when oneof_index = index -> acc + 1
          | _ -> acc
        ) fields
      in
      let acc =
        match count > (if params.Parameters.singleton_oneof_as_option then 1 else 0) with
        | true -> IntSet.add index acc
        | false -> acc
      in
      (index + 1, acc)
    ) oneof_decls
  in

  let plain_fields =
    List.filter ~f:(function
      | FieldDescriptorProto.{ oneof_index = None; _ } -> true
      | FieldDescriptorProto.{ oneof_index = Some index; _ } -> not (IntSet.mem index oneofs)
    ) fields
  in

  let field_name_map =
    (* If the message specifies extensions, reserve the name here *)
    let name_map = match List.is_empty extension_range with
      | true -> StringMap.empty
      | false ->
        StringMap.singleton extensions_name extensions_name
    in

    let plain_field_names = List.filter_map ~f:(fun field -> field.FieldDescriptorProto.name) plain_fields in
    let oneof_names =
      List.filter_map ~f:(fun field -> field.OneofDescriptorProto.name) oneof_decls
      |> List.filteri ~f:(fun i _name -> IntSet.mem i oneofs)
    in
    (* Extend name mapping. The 'extensions_name' should already have been allocated *)
    Names.create_ocaml_mapping ~name_map ~mangle_f ~name_f:Names.field_name (plain_field_names @ oneof_names)
  in
  let oneofs =
    List.mapi ~f:(fun i oneof -> i, oneof) oneof_decls
    |> List.filter ~f:(fun (i, _) -> IntSet.mem i oneofs)
    |> List.map ~f:(fun (i, OneofDescriptorProto.{ name; _ }) ->
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
        |> Names.create_ocaml_mapping ~mangle_f ~name_f:Names.poly_constructor_name
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
      ({ name; ocaml_name }, Oneof oneofs)
    )
  in
  let plain_fields =
    List.map ~f:(fun FieldDescriptorProto.{ name; type_name; type'; _ } ->
      let name = Option.value_exn ~message:"All fields should have a name" name in
      let ocaml_name = StringMap.find name field_name_map in
      let type_ = match type' with
        | Some FieldDescriptorProto.Type.TYPE_MESSAGE -> type_name
        | _ -> None
      in
      { name; ocaml_name; }, Plain { type_ }
    ) plain_fields
  in
  let fields =
    let fields = plain_fields @ oneofs in
    match List.is_empty extension_range with
    | true -> fields
    | false -> ({ name = extensions_name; ocaml_name = extensions_name }, Plain { type_ = None }) :: fields
  in
  Message { map_type; fields }

let element_of_enum ~mangle_f EnumDescriptorProto.{ value; _ } =
  (* We need the name to come from parent. I.e. though the scope. *)
  let names = List.filter_map ~f:(fun EnumValueDescriptorProto.{ name; _ } -> name) value in
  let name_map = Names.create_ocaml_mapping ~mangle_f ~name_f:Names.constructor_name names in
  let constructors =
    List.map ~f:(fun name ->
      { name; ocaml_name = StringMap.find name name_map }
    ) names
  in
  Enum constructors

let element_of_service ~mangle_f ServiceDescriptorProto.{ method'; _ } =
  let name_map =
    List.filter_map ~f:(fun MethodDescriptorProto.{ name; _ } -> name) method'
    |> Names.create_ocaml_mapping ~mangle_f ~name_f:Names.method_name
  in
  let entries =
    List.map ~f:(fun MethodDescriptorProto.{ name; _ } ->
      let name = Option.value_exn ~message:"Methods must have a name" name in
      let ocaml_name = StringMap.find name name_map in
      { name; ocaml_name }
    ) method'
  in
  Service entries

let rec traverse_message ~params ~mangle_f ~scope map services descriptorproto =
  let DescriptorProto.{ nested_type; enum_type; extension = extensions; _ } = descriptorproto in
  (* Scope contains all messages *)
  let name_map =
    let message_names = List.filter_map ~f:(fun f -> f.DescriptorProto.name) nested_type in
    let enum_names = List.filter_map ~f:(fun e -> e.EnumDescriptorProto.name) enum_type in
    let service_names = List.filter_map ~f:(fun s -> s.ServiceDescriptorProto.name) services in
    Names.create_ocaml_mapping ~mangle_f ~name_f:Names.module_name (message_names @ enum_names @ service_names)
  in
  (* Scope contains this element *)
  let message_element = element_of_message ~params ~mangle_f descriptorproto in

  (* Extension name should not interfere with other module names, but should still be uniq *)
  let extension_names = List.filter_map ~f:(fun e -> e.FieldDescriptorProto.name) extensions in
  let name_map =
    Names.create_ocaml_mapping ~name_map ~mangle_f ~name_f:Names.module_name extension_names
  in

  let map =
    List.fold_left ~init:map ~f:(fun map extension ->
      let proto_name = Option.value_exn ~message:"Enums must have a name" extension.FieldDescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      StringMap.add ~key:scope.proto_path ~data:(scope, Extension) map
    ) extensions
  in


  let map =
    List.fold_left ~init:map ~f:(fun map enum ->
      let proto_name = Option.value_exn ~message:"Enums must have a name" enum.EnumDescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      let element = element_of_enum ~mangle_f enum in
      StringMap.add ~key:scope.proto_path ~data:(scope, element) map
    ) enum_type
  in

  let map =
    List.fold_left ~init:map ~f:(fun map message ->
      let proto_name = Option.value_exn ~message:"All messages must have a name" message.DescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      let map, message_element = traverse_message ~params ~mangle_f ~scope map [] message in
      StringMap.add ~key:scope.proto_path ~data:(scope, message_element) map
    ) nested_type
  in
  let map =
    List.fold_left ~init:map ~f:(fun map service ->
      let proto_name = Option.value_exn ~message:"All services must have a name" service.ServiceDescriptorProto.name in
      let ocaml_name = StringMap.find proto_name name_map in
      let scope = add_scope ~proto_name ~ocaml_name scope in
      let element = element_of_service ~mangle_f service in
      StringMap.add ~key:scope.proto_path ~data:(scope, element) map
    ) services
  in
  map, message_element

let traverse_file ~params map module_name FileDescriptorProto.{ message_type = messages; package; enum_type = enums; service = services; extension = extensions; options; _ } =
  let mangle_f = match (Names.has_mangle_option options) with
    | true -> Names.to_snake_case
    | false -> fun x -> x
  in
  let default_scope = { proto_path = ""; ocaml_path = ""; ocaml_name = ""; module_name } in
  let scope, map =
    match package with
    | None -> default_scope, map
    | Some package ->
      List.fold_left
        ~init:(default_scope, map)
        ~f:(
          fun (scope, map) proto_name ->
            let ocaml_name = proto_name |> mangle_f |> Names.module_name in
            let scope = add_scope ~proto_name ~ocaml_name scope in
            (* Add the name to the map *)
            let map = StringMap.add ~key:scope.proto_path ~data:(scope, Package) map in
            (scope, map)
        )
        (String.split_on_char ~sep:'.' package)
  in
  (* Mimic a message. *)
  let message = DescriptorProto.make ~nested_type:messages ~enum_type:enums ~extension:extensions () in
  let map, _ = traverse_message ~params ~mangle_f map ~scope services message in
  map

(** Construct a set of proto_names (types) that are cyclic *)
let create_cyclic_set map =
  (* Create a map of dependencies for each type.
     This set only notes dependencies for types with only one field or messages that are maps
  *)
  let dependencies =
    StringMap.fold ~init:StringMap.empty ~f:(fun ~key ~data dependencies ->
      let deps =
        match snd data with
        | Message { map_type = Some _; fields } ->
          List.filter_map ~f:(function
            | _, Plain { type_ } -> type_
            | _, Oneof _ -> failwith "Oneofs cannot appear in a map"
          ) fields
        | Message { map_type = None; fields = [ _, Plain { type_ = Some type_ }] } -> [ type_ ]
        | Message { map_type = None; fields = [ _, Oneof [ { type_ = Some type_; _ } ]] } -> [ type_ ]
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
      let deps = match StringMap.find_opt proto_name dependencies with
        | None -> failwith_f "Could not find dependencies for type: %s" proto_name
        | Some deps -> deps
      in
      List.exists ~f:(is_cyclic seen) deps
  in
  StringMap.fold ~init:StringSet.empty ~f:(fun ~key ~data:_ cyclic_set ->
    match is_cyclic StringSet.empty key with
    | true -> StringSet.add key cyclic_set
    | false -> cyclic_set
  ) map

let make_module_name ~prefix_module_names ?package name =
  let package = match prefix_module_names with
    | false -> None
    | true -> package
  in
  Names.module_name_of_proto ?package name

let dump { map; cyclic_set = _; file_map } =
  let eprintf = Printf.eprintf in

  StringMap.iter ~f:(fun ~key ~data -> eprintf "Module %s: %s\n" key data) file_map;
  (* Just traverse everything *)
  StringMap.iter ~f:(fun ~key:proto_path ~data:({module_name; _ }, element_type) ->
    match element_type with
    | Message { fields; _ } ->
      List.iter ~f:(fun ({name; ocaml_name }, _field) ->
        eprintf "%s%s.%s -> %s\n" module_name proto_path name ocaml_name
      ) fields
    | _ -> ()
  ) map

let init ~params (files : FileDescriptorProto.t list) =
  let map, file_map = List.fold_left ~init:(StringMap.empty, StringMap.empty) ~f:(
    fun (map, file_map) file ->
      let file_name = Option.value_exn ~message:"Files must have a name" file.FileDescriptorProto.name in
      let module_name = make_module_name ~prefix_module_names:params.Parameters.prefix_output_with_package ?package:file.package file_name in
      let file_map = StringMap.add ~key:file_name ~data:module_name file_map in
      let map = traverse_file ~params map module_name file in
      (map, file_map)
  ) files in

  let cyclic_set = create_cyclic_set map in
  let t = { map; cyclic_set; file_map } in
  if false then dump t;
  t

(* The type map returns complete Ocaml names for modules. *)

(** [is_recursive t x] returns true if the message x is recursive and needs to be wrapped in a constructor *)
let is_cyclic { cyclic_set; _ } proto_name = StringSet.mem proto_name cyclic_set

(** Get the ocaml name of a message *)
let get_message_name { map; _ } ~proto_path name =
  let proto_name = sprintf "%s.%s" proto_path name in
  match StringMap.find_opt proto_name map with
  | Some ({ ocaml_name; _ }, Message _) -> ocaml_name
  | Some _ -> failwith_f "%s is not a message" proto_name
  | None -> failwith_f "%s not found" proto_name

(** Get the ocaml name of a field in a message *)
let get_message_field { map; _ } ~proto_path field_name =
  match StringMap.find_opt proto_path map with
  | None -> failwith_f "message %s not found" proto_path
  | Some (_, Message { fields; _ }) ->
    let field =
      List.find_opt ~f:(fun ({ name; _ }, _) -> field_name = name) fields
    in
    let name =
      match field with
      | Some ({ ocaml_name; _}, _) -> ocaml_name
      | None -> failwith_f "Field %s not found for message %s" field_name proto_path
    in
    name
  | _ -> failwith_f "%s is not a message" proto_path

(** The the ocaml name for a package *)
let get_package_name { map; _ } ~proto_path name =
  let proto_name = sprintf "%s.%s" proto_path name in
  match StringMap.find_opt proto_name map with
  | Some ({ ocaml_name; _ }, Package) -> ocaml_name
  | Some (_, element_type) -> failwith_f "%s is not a package but a %s" proto_name (string_of_element_type element_type)
  | None -> failwith_f "%s not found" proto_name

(** Get the name of a poly_constructor for a given field name in a oneof *)
let get_message_oneof_field { map; _ } ~proto_name ~oneof_name ~field_name =
  match StringMap.find_opt proto_name map with
  | None -> failwith_f "message %s not found" proto_name
  | Some (_, Message { fields; _ }) ->
    let field =
      List.find_map ~f:(function
        | { name; _ }, field when name = oneof_name -> Some field
        | _ -> None
      ) fields
    in
    let name =
      match field with
      | Some (Oneof oneofs) ->
        List.find_map ~f:(function
          | { name; constructor_name; _ } when name = field_name -> Some constructor_name
          | _ -> None
        ) oneofs
        |> Option.value_exn ~message:(sprintf "Field '%s' not part of oneof '%s' in message '%s'"
                                        field_name oneof_name proto_name)

      | Some (Plain _) -> failwith_f "Field '%s' in message %s is not a oneof field" field_name proto_name
      | None -> failwith_f "Field '%s' not found for message '%s'" oneof_name proto_name
    in
    name
  | _ -> failwith_f "'%s' is not a message" proto_name


(** Get the name of an enum *)
let get_enum_name { map; _ } ~proto_path ?name () =
  let proto_name = match name with
    | None -> proto_path
    | Some name -> sprintf "%s.%s" proto_path name
  in
  match StringMap.find_opt proto_name map with
  | Some ({ ocaml_name; _ }, Enum _) -> ocaml_name
  | Some (_, element_type) -> failwith_f "'%s' is not an enum but a '%s'" proto_name (string_of_element_type element_type)
  | None -> failwith_f "'%s' not found" proto_name

(** Get the name of an enum value (constructor) for a enum *)
let get_enum_value { map; _ } ~proto_path ?enum_name enum_value_name =
  let proto_name = match enum_name with
    | None -> proto_path
    | Some enum_name -> sprintf "%s.%s" proto_path enum_name
  in
  match StringMap.find_opt proto_name map with
  | None -> failwith_f "Enum '%s' not found" proto_name
  | Some (_, Enum values) -> begin
      List.find_opt values ~f:(fun { name; _ } -> name = enum_value_name)
      |> function
      | None -> failwith_f "Enum value '%s' not found in enum '%s'" enum_value_name proto_name
      | Some { ocaml_name; _ } -> ocaml_name
    end
  | Some (_, element_type) -> failwith_f "'%s(%s):%s' is of type '%s' and not type enum" proto_name (Option.value ~default:"<none>" enum_name) enum_value_name (string_of_element_type element_type)

(** Get the module name for a service *)
let get_service { map; _ } ~proto_path name =
  let proto_name = sprintf "%s.%s" proto_path name in
  match StringMap.find_opt proto_name map with
  | Some ({ ocaml_name; _ }, Service _) -> ocaml_name
  | Some (_, element_type) -> failwith_f "'%s' is not a service but a '%s'" proto_name (string_of_element_type element_type)
  | None -> failwith_f "'%s' not found" proto_name

(** Get the ocaml method name for a method in a service *)
let get_service_method { map; _ } ~proto_path ~service_name method_name =
  let proto_name = sprintf "%s.%s" proto_path service_name in
  match StringMap.find_opt proto_name map with
  | Some (_, Service methods) -> begin
      List.find_map ~f:(function
        | { name; ocaml_name } when method_name = name -> Some ocaml_name
        | _ -> None
      ) methods
      |> function
      | Some name -> name
      | None -> failwith_f "method '%s' not found in service '%s'" method_name proto_name
    end
  | Some (_, element_type) -> failwith_f "'%s' is not a method but a '%s'" proto_name (string_of_element_type element_type)
  | None -> failwith_f "'%s' not found" proto_name

let get_module_name { file_map; _ } proto_file =
  match StringMap.find_opt proto_file file_map with
  | None -> failwith_f "Could not find module name for '%s'" proto_file
  | Some module_name -> module_name

let get_location { map; _ } proto_path =
  match StringMap.find_opt proto_path map with
  | None -> failwith_f "Unknown proto_path '%s'" proto_path
  | Some ({ module_name; _}, _) -> module_name

let get_map_type { map; _ } proto_path =
  match StringMap.find_opt proto_path map with
  | Some (_, Message { map_type; _ }) -> map_type
  | Some (_, element_type) -> failwith_f "'%s' is not a message but a '%s'" proto_path (string_of_element_type element_type)
  | None -> failwith_f "message '%s' not found" proto_path

let get_entry { map; _ } proto_path =
  match StringMap.find_opt proto_path map with
  | Some (entry, _) -> entry
  | None -> failwith_f "%s not found" proto_path

(** [exists t proto_path] is true if the given proto_path is known
    and points to a structure *)
let exists { map; _ } proto_path =
  StringMap.mem proto_path map

let get_ocaml_path { map; _ } proto_path =
  match StringMap.find_opt proto_path map with
  | Some ({ ocaml_path = ""; ocaml_name; _ }, _) -> ocaml_name
  | Some ({ ocaml_path; ocaml_name; _ }, _ ) -> sprintf "%s.%s" ocaml_path ocaml_name
  | None -> failwith_f "type '%s' not found" proto_path

let get_extension { map; _ } ~proto_path name =
  let proto_name = sprintf "%s.%s" proto_path name in
  match StringMap.find_opt proto_name map with
  | Some ({ ocaml_name; _ }, Extension) -> ocaml_name
  | Some (_, element_type) -> failwith_f "'%s' is not an extension but a '%s'" proto_name (string_of_element_type element_type)
  | None -> failwith_f "'%s' not found" proto_name
