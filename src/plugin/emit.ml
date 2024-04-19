open StdLabels
open Parameters
open Spec.Descriptor.Google.Protobuf

module IntSet = Set.Make(struct type t = int let compare = compare end)
let sprintf = Printf.sprintf

(** Slightly overloaded name here.
    Its also used for all other types which would go into a module *)
type module' = {
  module_name : string;
  signature : Code.t;
  implementation : Code.t;
  deprecated : bool;
  comments : string list;
}

let emit_enum_type ~scope ~params ~comment_db
    EnumDescriptorProto.{name; value = values; options = options; reserved_range = _; reserved_name = _}
  : module' =
  let deprecated = match options with Some { deprecated; _ } -> deprecated | None -> false in
  let name = Option.value_exn ~message:"Enums must have a name" name in
  let module_name = Scope.get_name scope name in
  let signature = Code.init () in
  let implementation = Code.init () in
  let proto_path = Scope.get_proto_path scope in
  let scope = Scope.push scope name in
  let t = Code.init () in
  Code.emit t `Begin "type t = ";

  List.iter ~f:(fun EnumValueDescriptorProto.{ name; options; _ } ->
    let deprecated = match options with Some { deprecated; _ } -> deprecated | None -> false in
    let proto_path = Scope.get_proto_path scope in
    let enum_name =
      Scope.get_name_exn scope name
      |> Code.append_deprecaton_if `Attribute ~deprecated
    in
    Code.emit t `None "| %s" enum_name;
    Code.emit_comment ~position:`Trailing t
      (Comment_db.get_enum_value_comments comment_db ~proto_path ?name)
  ) values;
  Code.emit t `End "%s" params.Parameters.annot;

  Code.append signature t;
  Code.append implementation t;
  Code.emit signature `None "val name: unit -> string";
  Code.emit signature `None "(** Fully qualified protobuf name of this enum *)\n";
  Code.emit signature `None "(**/**)";
  Code.emit signature `None "val to_int: t -> int";
  Code.emit signature `None "val from_int: int -> t Runtime'.Result.t";
  Code.emit signature `None "val from_int_exn: int -> t";
  Code.emit signature `None "val to_string: t -> string";
  Code.emit signature `None "val from_string_exn: string -> t";
  Code.emit signature `None "(**/**)";


  Code.emit implementation `None "let name () = \"%s\"" (Scope.get_proto_path scope);
  Code.emit implementation `Begin "let to_int = function";
  List.iter ~f:(fun EnumValueDescriptorProto.{name; number; _} ->
    Code.emit implementation `None "| %s -> %d" (Scope.get_name_exn scope name) (Option.value_exn number)
  ) values;
  Code.emit implementation `EndBegin "let from_int_exn = function";
  let _ =
    List.fold_left ~init:IntSet.empty ~f:(fun seen EnumValueDescriptorProto.{name; number; _} ->
        let idx = (Option.value_exn ~message:"All enum descriptions must have a value" number) in
        match IntSet.mem idx seen with
        | true -> seen
        | false ->
          Code.emit implementation `None "| %d -> %s" idx (Scope.get_name_exn scope name);
          IntSet.add idx seen
      ) values
  in
  Code.emit implementation `None "| n -> Runtime'.Result.raise (`Unknown_enum_value n)";
  Code.emit implementation `End "let from_int e = Runtime'.Result.catch (fun () -> from_int_exn e)";
  Code.emit implementation `Begin "let to_string = function";
  List.iter ~f:(fun EnumValueDescriptorProto.{name; _} ->
    Code.emit implementation `None "| %s -> \"%s\"" (Scope.get_name_exn scope name) (Option.value_exn name)
  ) values;
  Code.emit implementation `EndBegin "let from_string_exn = function";
  List.iter ~f:(fun EnumValueDescriptorProto.{name; _} ->
    Code.emit implementation `None "| \"%s\" -> %s" (Option.value_exn name) (Scope.get_name_exn scope name)
  ) values;
  Code.emit implementation `None "| s -> Runtime'.Result.raise (`Unknown_enum_name s)";
  Code.emit implementation `End "";

  let comments = Comment_db.get_enum_comments comment_db ~proto_path ~name in
  { module_name; signature; implementation; deprecated; comments }

let emit_service_type ~options ~scope ~comment_db ServiceDescriptorProto.{ name; method' = methods; options = service_options; _ } =
  let emit_method signature implementation local_scope scope service_name MethodDescriptorProto.{ name; input_type; output_type; options = method_options; _} =
    let name = Option.value_exn name in
    let mangle_f = match Names.has_mangle_option options with
      | false -> fun id -> id
      | true -> Names.to_snake_case
    in
    let deprecated = match method_options with Some { deprecated; _ } -> deprecated | None -> false in

    let uncapitalized_name = mangle_f name |> String.uncapitalize_ascii |> Scope.Local.get_unique_name local_scope in
    (* To keep symmetry, only ensure that lowercased names are unique
       so that the upper case names are aswell.  We should remove this
       mapping if/when we deprecate the old API *)
    let capitalized_name = String.capitalize_ascii uncapitalized_name in

    let package_name_opt = Scope.get_package_name scope in
    let package_name =
      match package_name_opt with
        | None -> ""
        | Some p -> p ^ "."
    in
    let input = Scope.get_scoped_name scope input_type in
    let output = Scope.get_scoped_name scope output_type in
    let sig_t' =
      sprintf "(module Runtime'.Spec.Message with type t = %s.t) * (module Runtime'.Spec.Message with type t = %s.t)" input output
    in
    let proto_path = Scope.get_proto_path scope in
    Code.emit_comment ~position:`Leading signature (Comment_db.get_message_comments comment_db ~proto_path ~name);
    Code.emit signature `Begin "module %s : sig" capitalized_name;
    Code.emit signature `None "include Runtime'.Service.Rpc with type Request.t = %s.t and type Response.t = %s.t" input output;
    Code.emit signature `None "module Request : Runtime'.Spec.Message with type t = %s.t and type make_t = %s.make_t" input input;
    Code.emit signature `None "(** Module alias for the request message for this method call *)\n";

    Code.emit signature `None "module Response : Runtime'.Spec.Message with type t = %s.t and type make_t = %s.make_t" output output;
    Code.emit signature `None "(** Module alias for the response message for this method call *)\n";
    Code.emit signature `End "end%s" (Code.append_deprecaton_if ~deprecated `Item "");
    Code.emit signature `None "val %s : %s" uncapitalized_name sig_t';

    Code.emit implementation `Begin "module %s = struct" capitalized_name;
    Code.emit implementation `None "let package_name = %s" (Option.value_map ~default:"None" ~f:(fun n -> sprintf "Some \"%s\"" n) package_name_opt);
    Code.emit implementation `None "let service_name = \"%s\"" service_name;
    Code.emit implementation `None "let method_name = \"%s\"" name;
    Code.emit implementation `None "let name = \"/%s%s/%s\"" package_name service_name name;
    Code.emit implementation `None "module Request = %s" input;
    Code.emit implementation `None "module Response = %s" output;
    Code.emit implementation `End "end%s" (Code.append_deprecaton_if ~deprecated `Item "");
    Code.emit implementation `Begin "let %s : %s = " uncapitalized_name sig_t';
    Code.emit implementation `None "(module %s : Runtime'.Spec.Message with type t = %s.t ), " input input;
    Code.emit implementation `None "(module %s : Runtime'.Spec.Message with type t = %s.t )" output output;
    Code.emit implementation `End "";
  in
  let name = Option.value_exn ~message:"Service definitions must have a name" name in
  let deprecated = match service_options with Some { deprecated; _ } -> deprecated | None -> false in

  let proto_path = Scope.get_proto_path scope in
  let signature = Code.init () in
  let implementation = Code.init () in
  Code.emit_comment ~position:`Leading signature (Comment_db.get_service_comments comment_db ~proto_path ~name);
  Code.emit signature `Begin "module %s : sig" (Scope.get_name scope name);
  Code.emit implementation `Begin "module %s = struct" (Scope.get_name scope name);
  let local_scope = Scope.Local.init () in

  List.iter ~f:(emit_method signature implementation local_scope (Scope.push scope name) name) methods;
  Code.emit signature `End "end%s" (Code.append_deprecaton_if ~deprecated `Item "");
  Code.emit implementation `End "end%s" (Code.append_deprecaton_if ~deprecated `Item "");
  signature, implementation

let emit_extension ~scope ~params ~comment_db field =
  let FieldDescriptorProto.{ name; extendee; options; _ } = field in
  let deprecated = match options with Some { deprecated; _ } -> deprecated | None -> false in
  let name = Option.value_exn ~message:"Extensions must have a name" name in
  let module_name = (Scope.get_name scope name) in
  let extendee_type = Scope.get_scoped_name scope ~postfix:"t" extendee in
  let extendee_field = Scope.get_scoped_name scope ~postfix:"extensions'" extendee in
  (* Get spec and type *)
  let c =
    let params = Parameters.{params with singleton_record = false} in
    Types.spec_of_field ~params ~syntax:`Proto2 ~scope ~map_type:None field
  in
  let signature = Code.init () in
  let implementation = Code.init () in
  Code.append implementation signature;

  Code.emit signature `None "type t = %s %s" c.typestr params.annot;
  Code.emit signature `None "val get_exn: %s -> %s" extendee_type c.typestr;
  Code.emit signature `None "val get: %s -> (%s, [> Runtime'.Result.error]) result" extendee_type c.typestr;
  Code.emit signature `None "val set: %s -> %s -> %s" extendee_type c.typestr extendee_type;

  Code.emit implementation `None "module This = %s" module_name;
  Code.emit implementation `None "type t = %s %s" c.typestr params.annot;
  Code.emit implementation `None "let get_exn extendee = Runtime'.Extensions.get Runtime'.Spec.(%s) (extendee.%s)" c.spec_str extendee_field ;
  Code.emit implementation `None "let get extendee = Runtime'.Result.catch (fun () -> get_exn extendee)";
  Code.emit implementation `Begin "let set extendee t =";
  Code.emit implementation `None "let extensions' = Runtime'.Extensions.set Runtime'.Spec.(%s) (extendee.%s) t in" c.spec_str extendee_field;
  Code.emit implementation `None "{ extendee with %s = extensions' } [@@warning \"-23\"]" extendee_field;
  Code.emit implementation `End "";

  let comments = Comment_db.get_extension_comments comment_db ~proto_path:(Scope.get_proto_path scope) ~name in
  { module_name; signature; implementation; deprecated; comments }

(** Emit the nested types. *)
let emit_sub dest ~is_implementation ~is_first { module_name; signature; implementation; deprecated; comments } =
  if not is_implementation then Code.emit_comment ~position:`Leading dest comments;
  let () =
    match is_first with
    | true -> Code.emit dest `Begin "module rec %s : sig" module_name
    | false -> Code.emit dest `Begin "and %s : sig" module_name
  in
  Code.append dest signature;
  let () =
    match is_implementation with
    | false -> ()
    | true ->
      Code.emit dest `EndBegin "end = struct ";
      Code.emit dest `None "module %s = %s" Scope.this_module_alias module_name;
      Code.append dest implementation
  in
  Code.emit dest `End "end%s" (Code.append_deprecaton_if ~deprecated `Item "");
  ()

let rec emit_nested_types ~syntax ~signature ~implementation ?(is_first = true) nested_types =
  match nested_types with
  | [] -> ()
  | sub :: subs ->
    emit_sub ~is_implementation:false signature ~is_first sub;
    emit_sub ~is_implementation:true implementation ~is_first sub;
    emit_nested_types ~syntax ~signature ~implementation ~is_first:false subs

(** Test if the field referenced is a map type *)
let find_map_type ~scope field nested_types =
  match field with
  | FieldDescriptorProto.{ type' = Some TYPE_MESSAGE; label = Some Label.LABEL_REPEATED; type_name = Some type_name; _} ->
    let current_path = Scope.get_proto_path scope ^ "." in
    List.find_opt ~f:(function
      | DescriptorProto.{ name = Some name; options = Some { map_entry = Some true; _ }; _ } ->
        String.equal (current_path ^ name) type_name
      | _ -> false
    ) nested_types
  | _ -> None

(* Emit a message plus all its subtypes. *)
let rec emit_message ~params ~syntax ~scope ~comment_db
    DescriptorProto.{ name; field = fields; extension = extensions;
                      nested_type = nested_types; enum_type = enum_types;
                      extension_range = extension_ranges; oneof_decl = oneof_decls;
                      reserved_range = _; reserved_name = _; options = options } : module' =

  let signature = Code.init () in
  let implementation = Code.init () in
  let deprecated = match options with Some { deprecated; _ } -> deprecated | None -> false in

  let extension_ranges =
    List.map ~f:(function
      | DescriptorProto.ExtensionRange.{ start = Some start; end' = Some end'; _ } -> (start, end')
      | _ -> failwith "Start and end must be defined for Extension ranges"
    ) extension_ranges
  in
  (* Ignore empty modules *)
  let module_name, scope =
    match name with
    | None -> "", scope
    | Some name ->
      let module_name = Scope.get_name scope name in
      module_name, Scope.push scope name
  in
  (* Filter map types *)
  let nested_types_no_map = List.filter ~f:(function DescriptorProto.{ options = Some { map_entry = Some true; _ }; _ } -> false | _ -> true) nested_types in
  List.map ~f:(emit_enum_type ~scope ~comment_db~params) enum_types
  @ List.map ~f:(emit_message ~scope ~comment_db~params ~syntax) nested_types_no_map
  @ List.map ~f:(emit_extension ~scope ~comment_db ~params) extensions
  |> emit_nested_types ~syntax ~signature ~implementation;

  let () =
    match name with
    | Some _name ->
      let is_cyclic = Scope.is_cyclic scope in
      (* Map fields to denote if they are map types *)
      let fields = List.map ~f:(fun field -> field, find_map_type ~scope field nested_types) fields in
      let Types.{ type'; destructor; args; spec_str;
                  default_constructor_sig; default_constructor_impl; merge_impl } =
        Types.make ~params ~syntax ~is_cyclic ~extension_ranges ~scope ~comment_db ~fields oneof_decls
      in
      Code.emit signature `None "type t = %s%s" type' params.annot;
      Code.emit signature `None "val make: %s" default_constructor_sig;
      Code.emit signature `None "(** Helper function to generate a message using default values *)\n";

      Code.emit signature `None "val to_proto: t -> Runtime'.Writer.t";
      Code.emit signature `None "(** Serialize the message to binary format *)\n";

      Code.emit signature `None "val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result";
      Code.emit signature `None "(** Deserialize from binary format *)\n";
      Code.emit signature `None "val to_json: Runtime'.Json_options.t -> t -> Runtime'.Json.t";
      Code.emit signature `None "(** Serialize to Json (compatible with Yojson.Basic.t) *)\n";
      Code.emit signature `None "val from_json: Runtime'.Json.t -> (t, [> Runtime'.Result.error]) result";
      Code.emit signature `None "(** Deserialize from Json (compatible with Yojson.Basic.t) *)\n";

      Code.emit signature `None "val name: unit -> string";
      Code.emit signature `None "(** Fully qualified protobuf name of this message *)\n";

      Code.emit signature `None "(**/**)";
      Code.emit signature `None "type make_t = %s" default_constructor_sig;
      Code.emit signature `None "val merge: t -> t -> t";
      Code.emit signature `None "val to_proto': Runtime'.Writer.t -> t -> unit";
      Code.emit signature `None "val from_proto_exn: Runtime'.Reader.t -> t";
      Code.emit signature `None "val from_json_exn: Runtime'.Json.t -> t";
      Code.emit signature `None "(**/**)";

      Code.emit implementation `None "let name () = \"%s\"" (Scope.get_proto_path scope);
      Code.emit implementation `None "type t = %s%s" type' params.annot;

      Code.emit implementation `None "type make_t = %s" default_constructor_sig;
      Code.emit implementation `None "let make %s" default_constructor_impl;
      Code.emit implementation `None "let merge = \n%s" merge_impl;
      Code.emit implementation `None "let spec () = %s" spec_str;

      Code.emit implementation `Begin "let to_proto' =";
      Code.emit implementation `None "let serialize = Runtime'.Serialize.serialize (spec ()) in";
      Code.emit implementation `None "fun writer %s -> serialize writer %s" destructor (String.concat ~sep:" " args);
      Code.emit implementation `End "";

      Code.emit implementation `None "let to_proto t = let writer = Runtime'.Writer.init () in to_proto' writer t; writer";

      Code.emit implementation `Begin "let from_proto_exn =";
      Code.emit implementation `None "let constructor %s = %s in" (String.concat ~sep:" " args) destructor;
      Code.emit implementation `None "Runtime'.Deserialize.deserialize (spec ()) constructor";
      Code.emit implementation `End "let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)";
      Code.emit implementation `Begin "let to_json options = ";
      Code.emit implementation `None "let serialize = Runtime'.Serialize_json.serialize ~message_name:(name ()) (spec ()) options in";
      Code.emit implementation `None "fun %s -> serialize %s" destructor (String.concat ~sep:" " args);
      Code.emit implementation `EndBegin "let from_json_exn =";
      Code.emit implementation `None "let constructor %s = %s in" (String.concat ~sep:" " args) destructor;
      Code.emit implementation `None "Runtime'.Deserialize_json.deserialize ~message_name:(name ()) (spec ()) constructor";
      Code.emit implementation `End "let from_json json = Runtime'.Result.catch (fun () -> from_json_exn json)";
    | None -> ()
  in
  let comments = Comment_db.get_message_comments comment_db ~proto_path:(Scope.get_proto_path scope) in
  { module_name; signature; implementation; deprecated; comments }

let rec wrap_packages ~params ~syntax ~options ~comment_db ~scope message_type services = function
  | [] ->
    let { module_name = _; implementation; signature; deprecated = _; comments = _ } = emit_message ~params ~syntax ~scope ~comment_db message_type in
    List.iter ~f:(fun service ->
      let signature', implementation' = emit_service_type ~options ~scope ~comment_db service in
      Code.append implementation implementation';
      Code.append signature signature';
      ()
    ) services;
    signature, implementation

  | package :: packages ->
    let signature = Code.init () in
    let implementation = Code.init () in
    let package_name = Scope.get_name scope package in
    let scope = Scope.push scope package in

    let signature', implementation' =
      wrap_packages ~params ~syntax ~options ~scope ~comment_db message_type services packages
    in

    Code.emit implementation `Begin "module rec %s : sig" package_name;
    Code.append implementation signature';
    Code.emit implementation `EndBegin "end = struct";
    Code.append implementation implementation';
    Code.emit implementation `End "end";
    Code.emit signature `Begin "module rec %s : sig" package_name;
    Code.append signature signature';
    Code.emit signature `End "end";

    signature, implementation

let emit_header implementation ~proto_name ~syntax ~deprecated ~params =
  Code.emit implementation `None "(********************************************************)";
  Code.emit implementation `None "(*           AUTOGENERATED FILE - DO NOT EDIT!          *)";
  Code.emit implementation `None "(********************************************************)";
  Code.emit implementation `None "(* Generated by: ocaml-protoc-plugin                    *)";
  Code.emit implementation `None "(* https://github.com/andersfugmann/ocaml-protoc-plugin *)";
  Code.emit implementation `None "(********************************************************)";
  Code.emit implementation `None "(*";
  Code.emit implementation `None "  Source: %s" proto_name;
  Code.emit implementation `None "  Syntax: %s" (match syntax with `Proto2 -> "proto2" | `Proto3 -> "proto3");
  Code.emit implementation `None "  Parameters:";
  Code.emit implementation `None "    debug=%b" params.debug;
  Code.emit implementation `None "    annot='%s'" params.annot;
  Code.emit implementation `None "    opens=[%s]" (String.concat ~sep:"; " params.opens);
  Code.emit implementation `None "    int64_as_int=%b" params.int64_as_int;
  Code.emit implementation `None "    int32_as_int=%b" params.int32_as_int;
  Code.emit implementation `None "    fixed_as_int=%b" params.fixed_as_int;
  Code.emit implementation `None "    singleton_record=%b" params.singleton_record;
  Code.emit implementation `None "    prefix_output_with_package=%b" params.prefix_output_with_package;
  Code.emit implementation `None "*)";
  Code.emit implementation `None "[@@@ocaml.alert \"-protobuf\"] (* Disable deprecation warnings for protobuf*)";
  Code.emit implementation `None "%s" (Code.append_deprecaton_if ~deprecated `Floating "");
  ()

let parse_proto_file ~params ~scope filedescriptorproto =
  let FileDescriptorProto.{ name = proto_name; package; dependency = dependencies;
                            public_dependency = _;
                            weak_dependency = _; message_type = message_types;
                            enum_type = enum_types; service = services; extension;
                            options; source_code_info = _; syntax; } = filedescriptorproto
  in
  let comment_db = Comment_db.init filedescriptorproto in
  let proto_name = Option.value_exn ~message:"All files must have a name" proto_name in
  let syntax = match syntax with
    | None | Some "proto2" -> `Proto2
    | Some "proto3" -> `Proto3
    | _ -> failwith "Unsupported syntax"
  in
  let message_type =
    DescriptorProto.{name = None; nested_type=message_types; enum_type = enum_types;
                     field = []; extension; extension_range = []; oneof_decl = [];
                     options = None; reserved_range = []; reserved_name = []; }
  in
  let deprecated = match options with Some { deprecated; _ } -> deprecated | None -> false in
  let implementation = Code.init () in
  emit_header implementation ~proto_name ~syntax ~deprecated ~params;
  List.iter ~f:(Code.emit implementation `None "open %s [@@warning \"-33\"]" ) params.opens;
  Code.emit implementation `None "(**/**)";
  Code.emit implementation `None "module Runtime' = Ocaml_protoc_plugin [@@warning \"-33\"]";
  Code.emit implementation `Begin "module %s = struct" Scope.import_module_name;

  List.iter ~f:(fun dependency ->
    let module_name = Scope.get_module_name ~filename:dependency scope in
    Code.emit implementation `None "module %s = %s" module_name module_name;
  ) dependencies;
  Code.emit implementation `End "end";
  Code.emit implementation `None "(**/**)";

  let _signature', implementation' =
    wrap_packages ~params ~syntax ~options ~scope ~comment_db message_type services (Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package)
  in

  Code.append implementation implementation';
  Code.emit implementation `None "";

  let output_file_name =
    Scope.get_module_name scope ~filename:proto_name
    |> String.uncapitalize_ascii
    |> Printf.sprintf "%s.ml"
  in
  output_file_name, implementation
