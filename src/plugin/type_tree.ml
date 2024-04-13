open !StdLabels
open !MoreLabels
open !Utils

open Spec.Descriptor.Google.Protobuf


type t = { name: string;
           types: t list;
           depends: string list;
           fields: string list * string list list;
           enum_names: string list;
           service_names: string list
         }

type element = { module_name: string; (** The name of the module that holds the implementation (i.e. Ocaml module name of the generated ocaml file) *)
                 ocaml_name: string; (** Ocaml name of this type inside the module *)
                 cyclic: bool; (** True if the element contains cyclic references, in which case the type cannot be represented as a tuple *)
                 comments: string list;
               }

type file = { file_name: string; types: t list; package: string option }

let map_elements ~f ~comment_db ~path tpe elements =
  List.mapi ~f:(fun index element ->
    let path = (tpe, index) :: path in
    f ~comment_db ~path element
  ) elements

let module_name_of_proto ?package file =
  Filename.chop_extension file
  |> Filename.basename
  |> (
    match package with
    | Some package -> Printf.sprintf "%s_%s" package
    | None -> fun s -> s
  )
  |> String.capitalize_ascii
  |> String.map ~f:(function '-' | '.' -> '_' | c -> c)


let map_enum EnumDescriptorProto.{ name; value = values; _ } =
  let name = Option.value_exn ~message:"All enums must have a name" name in
  let enum_names =
    List.map ~f:(fun EnumValueDescriptorProto.{ name; _ } ->
      Option.value_exn ~message:"All enum values must have a name" name
    ) values
  in
  (* So this just returns all the enum names. For each enum name, we should add documentation *)
  { name; types = []; depends = []; fields = [], []; enum_names; service_names = [] }

let map_service ServiceDescriptorProto.{ name; method' = methods; _ } =
  let name = Option.value_exn ~message:"All enums must have a name" name in
  let service_names =
    List.map ~f:(fun MethodDescriptorProto.{ name; _ } ->
      Option.value_exn ~message:"All service methods must have a name" name
    ) methods
  in
  { name; types = []; depends = []; fields = [], []; enum_names = []; service_names}

let map_extension FieldDescriptorProto.{ name; _ } =
  let name = Option.value_exn ~message:"All enums must have a name" name in
  { name; types = []; depends = []; fields = [], []; enum_names = []; service_names = []}

let split_oneof_fields fields =
  let rec group acc ~eq = function
    | [] when acc = [] -> []
    | [] -> [ List.rev acc ]
    | x1 :: (x2 :: _  as xs) when eq x1 x2 -> group (x1 :: acc) ~eq xs
    | x :: xs -> (List.rev (x :: acc)) :: group [] ~eq xs
  in
  let field_number_of_field = function
    | FieldDescriptorProto.{ oneof_index = None; _ } -> failwith "Only oneof fields here"
    | FieldDescriptorProto.{ oneof_index = Some number; _ } -> number
  in

  let fields = List.sort ~cmp:(fun a b -> compare (field_number_of_field a) (field_number_of_field b)) fields in
  group [] ~eq:(fun a b -> field_number_of_field a = field_number_of_field b) fields


let rec map_message DescriptorProto.{ name; field = fields; nested_type = nested_types; enum_type = enums; oneof_decl = oneof_decls; extension = extensions; _} : t =
  let name = Option.value_exn ~message:"All messages must have a name" name in
  let depends =
    List.fold_left ~init:[] ~f:(fun acc -> function
      | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_MESSAGE; _ } ->
        type_name :: acc
      | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_ENUM; _ } ->
        type_name :: acc
      | _ -> acc
    ) fields
  in
  let enums =
    List.map ~f:map_enum enums
  in
  let extensions =
    List.map ~f:map_extension extensions
  in
  (* Need the full context. So we need to append more here, or strip the comment_db *)
  let nested_types =
    List.map ~f:map_message nested_types
  in
  let types = List.sort ~cmp:compare (enums @ extensions @ nested_types) in
  let fields =
    let field_name FieldDescriptorProto.{ name; _} =
      Option.value_exn ~message:"Field names cannot be null" name
    in
    let (plain_fields, oneof_fields) = List.partition ~f:(function FieldDescriptorProto.{ proto3_optional = Some true; _ } -> true
                                                                 | { oneof_index = None; _ } -> true
                                                                 | _ -> false) fields in
    let plain_fields =
      let acc = List.map ~f:field_name plain_fields in
      List.fold_left ~init:acc ~f:(fun acc OneofDescriptorProto.{ name; _ } ->
        (Option.value_exn ~message:"Oneof names cannot be null" name) :: acc
      ) oneof_decls
    in
    let oneof_fields =
      split_oneof_fields oneof_fields
      |> List.map ~f:(List.map ~f:field_name)
    in
    plain_fields, oneof_fields
  in
  { name; types; depends; fields; enum_names = []; service_names = [] }

let map_file filedescriptor =
  let FileDescriptorProto.{ name; message_type = messages; package; enum_type = enums; service = services; extension = extensions; _ } = filedescriptor in
  let messages = List.map ~f:map_message messages in
  let enums = List.map ~f:map_enum enums in
  let services = List.map ~f:map_service services in
  let extensions = List.map ~f:map_extension extensions in
  let types = enums @ messages @ services @ extensions in
  let file_name = Option.value_exn ~message:"File descriptor must have a name" name in
  let packages = Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package in

  let types = List.fold_right ~init:types ~f:(fun name types ->
    [ { name; types; depends = []; fields = [], []; enum_names = []; service_names = [] } ]) packages in
  (* What is a t list? *)
  { file_name; types; package }

let create_cyclic_map { file_name = _; types; package = _ } =
  let rec traverse path map { name; types; depends; _ } =
    let path = path ^ "." ^ name in
    let map = StringMap.add ~key:path ~data:(StringSet.of_list depends) map in
    List.fold_left ~init:map ~f:(traverse path) types
  in
  let is_cyclic map name =
    let rec inner name (seen : StringSet.t) =
      (* If a type has more than one depend, then the cyclic chain is broken, and
         we can stop processing further *)
      match StringMap.find_opt name map with
      | None -> seen
      | Some depends when StringSet.cardinal depends = 1 ->
        let unseen = StringSet.diff depends seen in
        let seen = StringSet.union depends seen in
        StringSet.fold ~init:seen ~f:inner unseen
      | Some _ -> seen
    in
    let seen = inner name StringSet.empty in
    StringSet.mem name seen
  in
  let map = List.fold_left ~init:StringMap.empty ~f:(traverse "") types in
  StringMap.mapi ~f:(fun name _ -> is_cyclic map name) map

(** Create a map: proto_name -> ocaml_name.
    Mapping is done in multiple passes to prioritize which mapping wins in case of name clashes
*)
let create_name_map ~standard_f ~mangle_f names =
  let rec uniq_name names ocaml_name =
    match List.assoc_opt ocaml_name names with
    | None -> ocaml_name
    | Some _ -> uniq_name names (ocaml_name ^ "'")
  in
  let names =
    List.map ~f:(fun name ->
      let mangle_name = mangle_f name in
      let standard_name = standard_f name in
      (name, mangle_name, standard_name)
    ) names
  in
  let standard_name_map =
    let inject ~f map =
      List.fold_left ~init:map ~f:(fun map (name, mangled_name, standard_name) ->
        match f name mangled_name standard_name with
        | true when StringMap.mem mangled_name map -> map
        | true -> StringMap.add ~key:mangled_name ~data:name map
        | false -> map
      ) names
    in
    StringMap.empty
    |> inject ~f:(fun name mangled_name _standard_name -> String.equal mangled_name name)
    |> inject ~f:(fun _name mangled_name standard_name -> String.equal mangled_name standard_name)
    |> inject ~f:(fun name mangled_name _standard_name -> String.equal (String.lowercase_ascii mangled_name) (String.lowercase_ascii name))
    |> inject ~f:(fun _name mangled_name standard_name -> String.equal (String.lowercase_ascii mangled_name) (String.lowercase_ascii standard_name))
  in
  List.fold_left ~init:[] ~f:(fun names (proto_name, ocaml_name, _) ->
    let ocaml_name =
      match StringMap.find_opt ocaml_name standard_name_map with
      | Some name when String.equal name proto_name -> ocaml_name
      | Some _ -> ocaml_name ^ "'"
      | None -> ocaml_name
    in
    (uniq_name names ocaml_name, proto_name) :: names
  ) names
  |> List.fold_left ~init:StringMap.empty ~f:(fun map (ocaml_name, proto_name) ->
    StringMap.add ~key:proto_name ~data:ocaml_name map
  )

(** Create a type db: map proto-type -> { module_name, ocaml_name, is_cyclic } *)
let create_file_db ~module_name ~mangle cyclic_map types =
  let mangle_f = match mangle with
    | true -> Names.to_snake_case
    | false -> fun x -> x
  in

  let add_names ~path ~ocaml_name map names =
    StringMap.fold ~init:map ~f:(fun ~key ~data map ->
      StringMap.add_uniq
        ~key:(path ^ "." ^ key)
        ~data:{ module_name; ocaml_name = ocaml_name ^ "." ^ data; cyclic = false; comments = []; }
        map
    ) names
  in

  let rec traverse_types map path types =
    let map_type ~map ~name_map path { name; types; fields = (plain_fields, oneof_fields); enum_names; service_names; _ } =
      let ocaml_name =
        let ocaml_name = StringMap.find name name_map in
        match StringMap.find path map with
        | { ocaml_name = ""; _ } -> ocaml_name
        | { ocaml_name = path; _ } -> path ^ "." ^ ocaml_name
      in
      let path = path ^ "." ^ name in
      let cyclic = StringMap.find path cyclic_map in
      let map =
        create_name_map
          ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
          ~mangle_f:(Names.field_name ~mangle_f)
          plain_fields
        |> add_names ~path ~ocaml_name map
      in
      let map =
        List.fold_left ~init:map ~f:(fun map fields ->
          create_name_map
            ~standard_f:(Names.poly_constructor_name ~mangle_f:(fun x -> x))
            ~mangle_f:(Names.poly_constructor_name ~mangle_f)
            fields
          |> add_names ~path ~ocaml_name map
        ) oneof_fields
      in
      let map =
        create_name_map
          ~standard_f:(Names.module_name ~mangle_f:(fun x -> x))
          ~mangle_f:(Names.module_name ~mangle_f)
          enum_names
        |> add_names ~path ~ocaml_name map
      in
      let map =
        create_name_map
          ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
          ~mangle_f:(Names.field_name ~mangle_f)
          service_names
        |> add_names ~path ~ocaml_name map
      in

      let map = StringMap.add_uniq ~key:path ~data:{ module_name; ocaml_name; cyclic; comments = [] } map in
      traverse_types map path types
    in
    let name_map =
      List.map ~f:(fun { name; _ } -> name) types
      |> create_name_map
           ~standard_f:(Names.module_name ~mangle_f:(fun x -> x))
           ~mangle_f:(Names.module_name ~mangle_f)
    in
    List.fold_left ~init:map ~f:(fun map type_ -> map_type ~map ~name_map path type_) types
  in

  let map = StringMap.singleton "" { ocaml_name = ""; module_name; cyclic = false; comments = [] } in
  traverse_types map "" types

let create_db ~prefix_module_names (files : FileDescriptorProto.t list) =
  let inner proto_file =
    let comment_db = Comment_db.make proto_file in
    let map = map_file proto_file in
    let module_name =
      let package = match prefix_module_names with
        | false -> None
        | true -> map.package
      in
      module_name_of_proto ?package map.file_name
    in
    let cyclic_map = create_cyclic_map map in
    let file_db =
      create_file_db ~module_name ~mangle:(Names.has_mangle_option proto_file.options) cyclic_map map.types
      |> StringMap.mapi ~f:(fun name element ->
        match StringMap.find_opt name comment_db with
        | None -> element
        | Some Comment_db.{ leading; trailing; _ } ->
          let comments = List.filter_map ~f:(fun x -> x) [leading; trailing] in
          { element with comments }
      )
    in
    (map.file_name, module_name), file_db
  in

  let names, maps = List.map ~f:inner files |> List.split in
  let name_map = List.to_seq names |> StringMap.of_seq in
  let file_map =
    List.fold_left ~init:StringMap.empty ~f:(
      StringMap.merge ~f:(fun _ a -> function
        | None -> a
        | b -> b
      )
    ) maps
  in
  name_map, file_map
