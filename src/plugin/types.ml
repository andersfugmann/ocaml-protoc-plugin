open StdLabels
(* This module is a bit elaborate.
   The idea is to construct the actual types needed
   in the spec module.

   This will ensure that the plugin will only construct valid types,
   so that changes to the spec will require changes here also.
*)


open Spec.Descriptor.Google.Protobuf

type type_modifier =
  | No_modifier of string (* The default value *)
  | Optional
  | List
  | Required
  | Oneof_type of string * (string * string) list

type type' =
  { name: string; modifier: type_modifier; deprecated: bool }

type c = {
  name : string;
  type' : type';
  spec_str: string;
}

type field_spec = {
  typestr : string;
  spec_str: string;
  deprecated: bool;
}

type t = {
  type' : string;
  constructor: string;
  apply: (string * string) option;
  spec_str: string;
  default_constructor_sig: string;
  default_constructor_impl: string;
  merge_impl: string;
}

(* Create module to hold textual representations for compound types. *)
module T = struct
  type _ message = { type': string; module_name: string }
  type _ enum = { type': string; module_name: string; default: string }
  type _ oneof = { type': string; spec: string; fields: (string * string) list }
  type _ oneof_elem = { adt_name: string }
  type _ map = { key_type: c; value_type: c }
end

open Ocaml_protoc_plugin.Spec.Make(T)


let sprintf = Printf.sprintf

let make_default: type a. a spec -> string -> a = function
  | Double -> float_of_string
  | Float -> float_of_string

  | Int32 -> Int32.of_string
  | UInt32 -> Int32.of_string
  | SInt32 -> Int32.of_string
  | Fixed32 -> Int32.of_string
  | SFixed32 -> Int32.of_string

  | Int32_int -> int_of_string
  | UInt32_int -> int_of_string
  | SInt32_int -> int_of_string
  | Fixed32_int -> int_of_string
  | SFixed32_int -> int_of_string

  | UInt64 -> Int64.of_string
  | Int64 -> Int64.of_string
  | SInt64 -> Int64.of_string
  | Fixed64 -> Int64.of_string
  | SFixed64 -> Int64.of_string

  | UInt64_int -> int_of_string
  | Int64_int -> int_of_string
  | SInt64_int -> int_of_string
  | Fixed64_int -> int_of_string
  | SFixed64_int -> int_of_string

  | Bool -> bool_of_string
  | String -> fun x -> x
  | Bytes -> Bytes.of_string
  | Enum _ -> fun _ ->
    (* Enum is 'a typed, so a default cannot be returned here as a string *)
    failwith (sprintf "Defaults for enums cannot be handled here")
  | Message _ -> failwith "Messages do not have defaults"

let string_of_default: type a. a spec -> a -> string = function
  | Double -> string_of_float
  | Float -> string_of_float

  | Int32 -> sprintf "%ldl"
  | UInt32 -> sprintf "%ldl"
  | SInt32 -> sprintf "%ldl"
  | Fixed32 -> sprintf "%ldl"
  | SFixed32 -> sprintf "%ldl"

  | Int32_int -> string_of_int
  | UInt32_int -> string_of_int
  | SInt32_int -> string_of_int
  | Fixed32_int -> string_of_int
  | SFixed32_int -> string_of_int

  | Int64 -> sprintf "%LdL"
  | UInt64 -> sprintf "%LdL"
  | SInt64 -> sprintf "%LdL"
  | Fixed64 -> sprintf "%LdL"
  | SFixed64 -> sprintf "%LdL"

  | UInt64_int -> string_of_int
  | Int64_int -> string_of_int
  | SInt64_int -> string_of_int
  | Fixed64_int -> string_of_int
  | SFixed64_int -> string_of_int

  | Bool -> string_of_bool
  | String -> sprintf "{|%s|}"
  | Bytes -> fun bytes -> sprintf "(Bytes.of_string {|%s|})" (Bytes.to_string bytes)
  | Enum { default; _ } -> fun _ -> default
  | Message _ -> failwith "Messages defaults are not relevant"

let default_of_spec: type a. a spec -> a = fun spec -> match spec with
  | Double -> 0.0
  | Float -> 0.0

  | Int32 -> 0l
  | UInt32 -> 0l
  | SInt32 -> 0l
  | Fixed32 -> 0l
  | SFixed32 -> 0l

  | Int32_int -> 0
  | UInt32_int -> 0
  | SInt32_int -> 0
  | Fixed32_int -> 0
  | SFixed32_int -> 0

  | Int64 -> 0L
  | UInt64 -> 0L
  | SInt64 -> 0L
  | Fixed64 -> 0L
  | SFixed64 -> 0L

  | UInt64_int -> 0
  | Int64_int -> 0
  | SInt64_int -> 0
  | Fixed64_int -> 0
  | SFixed64_int -> 0

  | Bool -> false
  | String -> ""
  | Bytes -> Bytes.of_string ""
  | Enum _-> failwith "Enums not handled here"
  | Message _ -> failwith "Messages defaults are not relevant"

let string_of_spec: type a. a spec -> string = function
  | Double -> "double"
  | Float -> "float"

  | Int32 -> "int32"
  | UInt32 -> "uint32"
  | SInt32 -> "sint32"
  | Fixed32 -> "fixed32"
  | SFixed32 -> "sfixed32"

  | Int32_int -> "int32_int"
  | UInt32_int -> "uint32_int"
  | SInt32_int -> "sint32_int"
  | Fixed32_int -> "fixed32_int"
  | SFixed32_int -> "sfixed32_int"

  | UInt64 -> "uint64"
  | Int64 -> "int64"
  | SInt64 -> "sint64"
  | Fixed64 -> "fixed64"
  | SFixed64 -> "sfixed64"

  | UInt64_int -> "uint64_int"
  | Int64_int -> "int64_int"
  | SInt64_int -> "sint64_int"
  | Fixed64_int -> "fixed64_int"
  | SFixed64_int -> "sfixed64_int"

  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "bytes"
  | Enum { module_name; _ }  -> sprintf "(enum (module %s))" module_name
  | Message { module_name; _ } -> sprintf "(message (module %s))" module_name

let type_of_spec: type a. a spec -> string = function
  | Double -> "float"
  | Float -> "float"

  | Int32 -> "int32"
  | UInt32 -> "int32"
  | SInt32 -> "int32"
  | Fixed32 -> "int32"
  | SFixed32 -> "int32"

  | Int32_int -> "int"
  | UInt32_int -> "int"
  | SInt32_int -> "int"
  | Fixed32_int -> "int"
  | SFixed32_int -> "int"

  | UInt64 -> "int64"
  | Int64 -> "int64"
  | SInt64 -> "int64"
  | Fixed64 -> "int64"
  | SFixed64 -> "int64"

  | UInt64_int -> "int"
  | Int64_int -> "int"
  | SInt64_int -> "int"
  | Fixed64_int -> "int"
  | SFixed64_int -> "int"

  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "bytes"
  | Enum { type'; _ } -> type'
  | Message { type'; _ } -> type'

let is_deprecated = function
  | FieldDescriptorProto.{ options = Some { deprecated; _ }; _ } -> deprecated
  | FieldDescriptorProto.{ options = None; _ } -> false

let spec_of_message ~scope type_name =
  let type' = Scope.get_scoped_name ~postfix:"t" scope type_name in
  let module_name = Scope.get_scoped_name scope type_name in
  Message { type'; module_name }

let spec_of_enum ~scope type_name default =
  let type' = Scope.get_scoped_name ~postfix:"t" scope type_name in
  let module_name = Scope.get_scoped_name scope type_name in
  let default =
    match default with
    | Some default ->
      Option.value_exn type_name
      |> (fun type_name -> sprintf "%s.%s" type_name default)
      |> Option.some
      |> Scope.get_scoped_name scope
    | None ->
      Scope.get_scoped_enum_name scope type_name
  in
  Enum { type'; module_name; default }

open Parameters
let spec_of_type ~params ~scope type_name default =
  let open FieldDescriptorProto.Type in
  function
  | TYPE_DOUBLE   -> Espec Double
  | TYPE_FLOAT    -> Espec Float

  | TYPE_INT64  when params.int64_as_int -> Espec Int64_int
  | TYPE_UINT64 when params.int64_as_int -> Espec UInt64_int
  | TYPE_SINT64 when params.int64_as_int -> Espec SInt64_int

  | TYPE_UINT32 when params.int32_as_int -> Espec UInt32_int
  | TYPE_INT32  when params.int32_as_int -> Espec Int32_int
  | TYPE_SINT32 when params.int32_as_int -> Espec SInt32_int

  | TYPE_FIXED32  when params.fixed_as_int -> Espec Fixed32_int
  | TYPE_SFIXED32 when params.fixed_as_int -> Espec SFixed32_int
  | TYPE_FIXED64  when params.fixed_as_int -> Espec Fixed64_int
  | TYPE_SFIXED64 when params.fixed_as_int -> Espec SFixed64_int

  | TYPE_INT64  -> Espec Int64
  | TYPE_UINT64 -> Espec UInt64
  | TYPE_SINT64 -> Espec SInt64

  | TYPE_UINT32 -> Espec UInt32
  | TYPE_INT32  -> Espec Int32
  | TYPE_SINT32 -> Espec SInt32

  | TYPE_FIXED32  -> Espec Fixed32
  | TYPE_SFIXED32 -> Espec SFixed32
  | TYPE_FIXED64  -> Espec Fixed64
  | TYPE_SFIXED64 -> Espec SFixed64

  | TYPE_BOOL     -> Espec Bool
  | TYPE_STRING   -> Espec String
  | TYPE_BYTES    -> Espec Bytes

  | TYPE_GROUP    -> failwith "Groups not supported"
  | TYPE_MESSAGE  -> Espec (spec_of_message ~scope type_name)
  | TYPE_ENUM     -> Espec (spec_of_enum ~scope type_name default)

let string_of_index (index, name, json_name) =
  sprintf "(%d, \"%s\", \"%s\")" index name json_name

let string_of_oneof_elem (Oneof_elem (index, spec, { adt_name; _ } ) ) =
  let spec_string = string_of_spec spec in
  let index_string = string_of_index index in
  let constr = sprintf "fun v -> %s v" adt_name in
  let destr = sprintf "function %s v -> v | _ -> raise (Invalid_argument \"Cannot destruct given oneof\")" adt_name in
  sprintf "oneof_elem (%s, %s, ((%s), (%s)))" index_string spec_string constr destr

let string_of_proto_type: type a. a spec -> a -> string = fun spec default ->
  sprintf "(%s)" (string_of_default spec default)

let string_of_packed = function
  | Packed -> "packed"
  | Not_packed -> "not_packed"

let string_of_type = function
  | { name; modifier = (No_modifier _ | Required | Oneof_type _); _ } -> name
  | { name; modifier = List; _ } -> sprintf "%s list" name
  | { name; modifier = Optional; _ } -> sprintf "%s option" name

let c_of_compound: type a. deprecated:bool -> string -> a compound -> c = fun ~deprecated name -> function
  | Basic (index, spec, default) ->
    let index_string = string_of_index index in
    let spec_str = sprintf "basic (%s, %s, %s)" index_string (string_of_spec spec) (string_of_proto_type spec default) in
    let modifier =
      match spec with
      | Message _ -> Optional
      | _ -> No_modifier (string_of_default spec default)
    in
    let type' = { name = type_of_spec spec; modifier; deprecated } in
    { name; type'; spec_str }
  | Basic_req (index, spec) ->
    let index_string = string_of_index index in
    let spec_str = sprintf "basic_req (%s, %s)" index_string (string_of_spec spec) in
    let type' = { name = type_of_spec spec; modifier = Required; deprecated } in
    { name; type'; spec_str }
  | Basic_opt (index, spec) ->
    let index_string = string_of_index index in
    let spec_str = sprintf "basic_opt (%s, %s)" index_string (string_of_spec spec) in
    let type' = { name = type_of_spec spec; modifier = Optional; deprecated } in
    { name; type'; spec_str }
  | Repeated (index, spec, packed) ->
    let index_string = string_of_index index in
    let spec_str = sprintf "repeated (%s, %s, %s)" index_string (string_of_spec spec) (string_of_packed packed) in
    let type' = { name = type_of_spec spec; modifier = List; deprecated } in
    { name; type'; spec_str; }
  | Map (index, { key_type; value_type } ) ->
    let index_string = string_of_index index in
    let spec_str = sprintf "map (%s, (%s, %s))" index_string key_type.spec_str value_type.spec_str in
    let type_name = sprintf "(%s * %s)" (string_of_type key_type.type') (string_of_type value_type.type') in
    let type' = { name = type_name; modifier = List; deprecated } in
    { name; type'; spec_str; }
  | Oneof { type'; spec; fields; _ } ->
    let spec_str = sprintf "oneof (%s)" spec in
    let type' = { name = type'; modifier = Oneof_type ({|`not_set|}, fields); deprecated } in
    { name; type'; spec_str }

let rec c_of_field ~params ~syntax ~scope ~map_type field =
  let open FieldDescriptorProto in
  let open FieldDescriptorProto.Type in
  let number = Option.value_exn field.number in
  let name = Option.value_exn field.name in
  let json_name = Option.value_exn field.json_name in
  let index = (number, name, json_name) in
  let deprecated = is_deprecated field in

  match syntax, field with
  (* This function cannot handle oneof types *)
  | _, { oneof_index = Some _; proto3_optional = Some false | None; _ } -> failwith "Cannot handle oneofs"
  (* Optional messages cannot have a default *)
  | _, { type' = Some TYPE_MESSAGE; default_value = Some _; _ } ->
    failwith "Message types cannot have a default value"
  (* Proto3 cannot have defaults *)
  | `Proto3, { default_value = Some _; _ } ->
    failwith "Default values illegal under proto3"
  (* Proto3 does not support required fields *)
  | `Proto3, { label = Some Label.LABEL_REQUIRED; _ } ->
    failwith "Required fields illegal under proto3"

  (* Optional message *)
  | _, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
    Basic_opt (index, spec)
    |> c_of_compound ~deprecated name

  (* Required message *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
    Basic_req (index, spec)
    |> c_of_compound ~deprecated name

  (* Enum under proto2 with a default value *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; default_value = Some default; _ } ->
    let spec = spec_of_enum ~scope type_name (Some default) in
    Basic (index, spec, default)
    |> c_of_compound ~deprecated name

  (* Enum under proto2 with no default value *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; default_value = None; _ } ->
    let spec = spec_of_enum ~scope type_name None in
    Basic_opt (index, spec)
    |> c_of_compound ~deprecated name

  (* Required Enum under proto2 *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some TYPE_ENUM; type_name; _ } ->
    let spec = spec_of_enum ~scope type_name None in
    Basic_req (index, spec)
    |> c_of_compound ~deprecated name

  (* Required fields under proto2 *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some type'; type_name; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_req (index, spec)
    |> c_of_compound ~deprecated name

  (* Proto2 optional fields with a default *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; default_value = Some default; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name (Some default) type' in
    let default = make_default spec default in
    Basic (index, spec, default)
    |> c_of_compound ~deprecated name

  (* Proto2 optional fields - no default *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; default_value = None; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_opt (index, spec)
    |> c_of_compound ~deprecated name

  (* Proto3 explicitly optional field are mapped as proto2 optional fields *)
  | _, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; proto3_optional = Some true; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_opt (index, spec)
    |> c_of_compound ~deprecated name

  (* Proto3 enum implicitly optional field *)
  | `Proto3, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; _} ->
    let spec, default =
      match spec_of_enum ~scope type_name None with
      | (Enum { default; _ }) as spec ->
        spec, default
      | _ -> failwith "Must be an enum spec"
    in
    Basic (index, spec, default)
    |> c_of_compound ~deprecated name

  (* Proto3 implicitly optional field *)
  | `Proto3, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; _} ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    let default = default_of_spec spec in
    Basic (index, spec, default)
    |> c_of_compound ~deprecated name

  (* Repeated fields cannot have a default *)
  | _, { label = Some Label.LABEL_REPEATED; default_value = Some _; _ } -> failwith "Repeated fields does not support default values"

  (* Repeated message - map type *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some Type.TYPE_MESSAGE; _ } when map_type != None ->
    let lookup n = function
      | Some DescriptorProto.{ field = fields; _ } -> List.find_opt ~f:(function { name = Some name; _ } -> String.equal name n | _ -> false) fields
      | None -> None
    in
    (* A Map type cannot be recursibe. And we actually have the type. So lets create the spec *)
    let key_type =
      lookup "key" map_type |> Option.value_exn ~message:"Maps must contain a key field"
      |> c_of_field ~params ~syntax ~scope ~map_type:None
    in
    let value_type =
      lookup "value" map_type |> Option.value_exn ~message:"Maps must contain a value field"
      |> c_of_field ~params ~syntax ~scope ~map_type:None
    in
    Map (index, { key_type; value_type }) (* The spec is not the same here *)
    |> c_of_compound ~deprecated name

  (* Repeated message *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some Type.TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
    Repeated (index, spec, Not_packed)
    |> c_of_compound ~deprecated name

  (* Repeated bytes and strings are not packed *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some (TYPE_STRING | TYPE_BYTES as type'); type_name; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Repeated (index, spec, Not_packed)
    |> c_of_compound ~deprecated name

  (* Repeated enum *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some Type.TYPE_ENUM; type_name; options; _} ->
    let spec = spec_of_enum ~scope type_name None in
    let packed = match syntax, options with
      | _, Some FieldOptions.{ packed = Some true; _ } -> Packed
      | _, Some FieldOptions.{ packed = Some false; _ } -> Not_packed
      | `Proto2, _ -> Not_packed
      | `Proto3, _ -> Packed
    in
    Repeated (index, spec, packed)
    |> c_of_compound ~deprecated name

  (* Repeated basic type *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some type'; type_name; options; _} ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    let packed = match syntax, options with
      | _, Some FieldOptions.{ packed = Some true; _ } -> Packed
      | _, Some FieldOptions.{ packed = Some false; _ } -> Not_packed
      | `Proto2, _ -> Not_packed
      | `Proto3, _ -> Packed
    in
    Repeated (index, spec, packed)
    |> c_of_compound ~deprecated name
  | _, { label = None; _ } -> failwith "Label not set on field struct"
  | _, { type' = None; _ } -> failwith "Type must be set"


let spec_of_field ~params ~syntax ~scope ~map_type field : field_spec =
  let c = c_of_field ~params ~syntax ~scope ~map_type field in
  {
    typestr = string_of_type c.type';
    spec_str = c.spec_str;
    deprecated = is_deprecated field;
  }

let c_of_oneof ~params ~syntax:_ ~scope OneofDescriptorProto.{ name; _ } fields =
  let open FieldDescriptorProto in
  (* Construct the type. *)
  let field_infos =
    List.map ~f:(function
      | { number = Some number; name = Some name; type' = Some type'; type_name; json_name = Some json_name; _ } as field, _map_type ->
        let index = (number, name, json_name) in
        let Espec spec = spec_of_type ~params ~scope type_name None type' in
        (index, Some name, type_of_spec spec, Espec spec, is_deprecated field)
      | _ -> failwith "No index or type"
    ) fields
  in
  let oneof =
    let oneof_elems =
      field_infos
      |>
      List.map ~f:(fun (index, name, _type', Espec spec, _deprecated) ->
        let adt_name = Scope.get_name_exn scope name in
        let arg : _ T.oneof_elem = { adt_name; } in
        adt_name, Oneof_elem (index, spec, arg)
      )
    in
    let type' =
      field_infos
      |> List.map ~f:(fun (_, name, type', _, deprecated) ->
        sprintf "%s of %s" (Scope.get_name_exn scope name) type'
        |> Code.append_deprecaton_if ~deprecated `Attribute
       )
      |> String.concat ~sep:" | "
      |> sprintf "[ `not_set | %s ]"
    in
    let oneofs =
      oneof_elems
      |> List.map ~f:snd
      |> List.map ~f:string_of_oneof_elem
      |> String.concat ~sep:"; "
      |> sprintf "[ %s ]"
    in
    let index_f =
      "| `not_set -> failwith \"Impossible case\"" ::
      List.mapi oneof_elems ~f:(fun index (adt_name, _) ->
        sprintf "%s _ -> %d" adt_name index
      )
      |> String.concat ~sep:" | "
      |> sprintf "(function %s)"
    in
    let fields =
      List.map oneof_elems ~f:(fun (name, Oneof_elem (_, spec, _)) ->
        name, string_of_spec spec
      )
    in
    let spec = sprintf "(%s, %s)" oneofs index_f in
    Oneof { type'; spec = spec; fields }
  in

  c_of_compound ~deprecated:false (Option.value_exn name) oneof

(** Return a list of plain fields + a list of fields per oneof_decl *)
let split_oneof_decl fields oneof_decls =
  let open FieldDescriptorProto in
  let rec filter_oneofs acc rest index = function
    | ({ oneof_index = Some i; _ }, _) as f :: fs when i = index ->
      filter_oneofs (f :: acc) rest index fs
    | f :: fs -> filter_oneofs acc (f :: rest) index fs
    | [] -> List.rev acc, List.rev rest
  in
  let rec inner = function
    | ({ oneof_index = Some i; _ }, _) as f :: fs ->
      let oneofs, fs = filter_oneofs [f] [] i fs in
      let decl = List.nth oneof_decls i in
      `Oneof (decl, oneofs) :: inner fs
    | f :: fs ->
      `Field f :: inner fs
    | [] -> []
  in
  inner fields

let sort_fields fields =
  let number = function
    | (FieldDescriptorProto.{ number = Some number; _ }, _) -> number
    | _ -> failwith "All Fields must have a number"
  in
  List.sort ~cmp:(fun v v' -> Int.compare (number v) (number v')) fields

let make ~params ~syntax ~is_cyclic ~extension_ranges ~scope ~fields oneof_decls =
  let fields = sort_fields fields in
  let ts =
    split_oneof_decl fields oneof_decls
    |> List.map ~f:(function
      (* proto3 Oneof fields with only one field is mapped as regular field *)
      | `Oneof (_, [ (FieldDescriptorProto.{ proto3_optional = Some true; _ } as field, map_type) ] )
      | `Field (field, map_type) -> c_of_field ~params ~syntax ~scope ~map_type field
      | `Oneof (decl, fields) -> c_of_oneof ~params ~syntax ~scope decl fields
    )
  in

  let has_extensions = match extension_ranges with [] -> false | _ -> true in

  let constructor_sig_arg = function
    | { name; type' = { name = type_name; modifier = Required; deprecated = _ }; _ } ->
      sprintf "%s:%s" (Scope.get_name scope name) type_name
    | { name; type' = { name = type_name; modifier = List; deprecated = _  }; _} ->
      sprintf "?%s:%s list" (Scope.get_name scope name) type_name
    | { name; type' = { name = type_name; modifier = (Optional | No_modifier _ | Oneof_type _); deprecated = _  }; _} ->
      sprintf "?%s:%s" (Scope.get_name scope name) type_name
  in
  let constructor_arg c =
    let name = Scope.get_name scope c.name in
    match c with
    | { type' = { modifier = Required; _}; _ } -> sprintf "~%s" name
    | { type' = { modifier = Optional; _ }; _} -> sprintf "?%s" name
    | { type' = { modifier = List; _ }; _} -> sprintf "?(%s = [])" name
    | { type' = { modifier = (No_modifier default | Oneof_type (default, _)); _}; _} -> sprintf "?(%s = %s)" name default
  in
  let prepend ?(cond=true) elm l = match cond with
    | true -> elm :: l
    | false -> l
  in
  let append ?(cond=true) elm l = match cond with
    | true -> l @ [elm]
    | false -> l
  in

  let t_as_tuple = List.length ts = 1 &&
                   params.singleton_record = false &&
                   not has_extensions &&
                   not is_cyclic
  in

  let has_deprecated_fields = List.exists ~f:(fun ({ type' = { deprecated; _ }; _ }: c) -> deprecated) ts in

  let type_constr field_infos = match t_as_tuple, field_infos with
    | _, [] -> "unit"
    | true, fields ->
      List.map ~f:snd fields |> List.map ~f:fst
      |> String.concat ~sep:" * "
      |> sprintf "(%s)"
      |> Code.append_deprecaton_if ~deprecated:has_deprecated_fields `Item
    | false, fields ->
      List.map ~f:(fun (name, (type', deprecated)) ->
        sprintf "%s: %s" name type'
        |> Code.append_deprecaton_if ~deprecated `Attribute
      ) fields
      |> String.concat ~sep:"; "
      |> sprintf "{ %s }"
  in

  let type_destr field_infos = match t_as_tuple, field_infos with
    | _, [] -> "()"
    | true, fields ->
      List.map ~f:fst fields
      |> String.concat ~sep:", "
      |> sprintf "(%s)"
    | false, fields ->
      List.map ~f:fst fields
      |> String.concat ~sep:"; "
      |> sprintf "{ %s }"
  in

  let field_info =
    List.rev_map ~f:(fun { name; type'; _} -> (Scope.get_name scope name, (string_of_type type', type'.deprecated)) ) ts
    |> prepend ~cond:has_extensions ("extensions'", ("Runtime'.Extensions.t", false))
    |> List.rev
  in
  let type' = type_constr field_info in

  let args =
    List.map ~f:fst field_info
    |> String.concat ~sep:" "
  in

  let constructor =
    let destr = type_destr field_info in
    match field_info with
    | [] -> destr
    | _ -> sprintf "fun %s -> %s" args destr
  in
  let apply = match t_as_tuple with
    | true -> None
    | false -> Some ((type_destr field_info), args)
  in

  let default_constructor_sig =
    List.rev_map ~f:constructor_sig_arg ts
    |> prepend ~cond:has_extensions "?extensions':Runtime'.Extensions.t"
    |> prepend "unit"
    |> prepend "t"
    |> List.rev
    |> String.concat ~sep:" -> "
  in
  let default_constructor_impl =
    let args =
      List.rev_map ~f:constructor_arg ts
      |> prepend ~cond:has_extensions "?(extensions' = Runtime'.Extensions.default)"
      |> prepend "()"
      |> List.rev
      |> String.concat ~sep: " "
    in

    let constructor = type_destr field_info in
    sprintf "%s = %s" args constructor
  in
  let nil =
    match has_extensions with
    | true ->
      extension_ranges
      |> List.map ~f:(fun (start', end') -> sprintf "(%d, %d)" start' end')
      |> String.concat ~sep:"; "
      |> sprintf "nil_ext [ %s ]"
    | false -> "nil"
  in
  (* Create the deserialize spec *)
  let spec_str =
    let spec = List.map ~f:(fun (c : c) -> c.spec_str) ts in
    String.concat ~sep:" ^:: " (spec @ [nil])
    |> sprintf "Runtime'.Spec.( %s )"
  in

  let merge_impl =
    let as_tuple = t_as_tuple || List.length ts = 0 && not has_extensions in
    let args =
      List.map ["t1";"t2"] ~f:(fun s ->
        match as_tuple with
        | true ->
          List.map ~f:(fun c -> sprintf "%s_%s" s (Scope.get_name scope c.name)) ts
          |> String.concat ~sep:","
          |> sprintf "(%s)"
        | false -> s
      )
      |> String.concat ~sep:" "
    in
    let sep = match as_tuple with true -> "_" | false -> "." in
    let merge_values =
      List.map ts ~f:(function
        | { name; type' = { modifier = Oneof_type (_, ctrs); _ }; _ } ->
          (* Default values for oneof fields makes absolutely no sense!.
             Consider a oneof type with two fields with a default value.
             Its undecidable if any should be marked as set if none of the fields
             are transmitted. The system should actually warn (or error) if
             a syntax2 oneof field is marked with a default value
          *)

          let name = Scope.get_name scope name in
          sprintf "match ((t1%s%s), (t2%s%s)) with"sep name sep name ::
          List.map ~f:(fun (ctr, type') ->
            let spec = sprintf "basic_req ((0, \"\", \"\"), %s)" type' in (* Oneof messages are marked as required, as one must be set. *)
            sprintf "  | (%s v1, %s v2) -> %s (Runtime'.Merge.merge Runtime'.Spec.( %s ) v1 v2)" ctr ctr ctr spec
          ) ctrs
          |> append "  | (v1, `not_set)  -> v1"
          |> append "  | (_, v2) -> v2"
          |> String.concat ~sep:"\n"
          |> fun value -> name, value

        | { name; spec_str; _ } ->
          let name = Scope.get_name scope name in
          name, sprintf "Runtime'.Merge.merge Runtime'.Spec.( %s ) t1%s%s t2%s%s"
            spec_str sep name sep name
      )
      |> append ~cond:has_extensions ("extensions'", sprintf "List.append t1%sextensions' t2%sextensions'" sep sep)
    in
    let constr =
      match as_tuple with
      | true ->
        List.map ~f:snd merge_values
        |> String.concat ~sep:","
        |> sprintf "(%s)"
      | false ->
        List.map merge_values ~f:(fun (name, value) ->
          Printf.sprintf "%s = (%s);" name value
        )
        |> String.concat ~sep:"\n"
        |> sprintf "{\n%s\n }"
    in
    sprintf "fun %s -> %s" args constr
  in

  { type'; constructor; apply; spec_str; default_constructor_sig; default_constructor_impl; merge_impl }
