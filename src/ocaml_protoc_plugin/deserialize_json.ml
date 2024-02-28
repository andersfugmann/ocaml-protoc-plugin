(** Module for deserializing values *)
open! StdLabels
open Spec

module FieldMap = Map.Make(String)
type fields = Yojson.Basic.t FieldMap.t

let value_error type_name json =
  Result.raise (`Wrong_field_type (type_name, Yojson.Basic.to_string json))

let to_int64 = function
  | `String s -> Int64.of_string s
  | `Int v -> Int64.of_int v
  | json -> value_error "int64" json
let to_int json = to_int64 json |> Int64.to_int
let to_int32 = function
  | `String s -> Int32.of_string s
  | `Int v -> Int32.of_int v
  | json -> value_error "int32" json
let to_string = function
  | `String s -> s
  | json -> value_error "string" json
let to_bytes json = to_string json |> Base64.decode_exn |> Bytes.of_string
let to_enum: type a. (module Spec.Enum with type t = a) -> Yojson.Basic.t -> a = fun (module Enum) -> function
  | `String enum -> Enum.from_string_exn enum
  | `Int enum -> Enum.from_int_exn enum
  | json -> value_error "enum" json

let to_float = function
  | `Float f -> f
  | `String s -> Float.of_string s
  | json -> value_error "float" json

let to_bool = function
  | `Bool b -> b
  | json -> value_error "bool" json

let to_list = function
  | `List l -> l
  | json -> value_error "list" json

let read_value: type a. a spec -> Yojson.Basic.t -> a = function
   | Double -> to_float
   | Float -> to_float
   | Int32 -> to_int32
   | UInt32 -> to_int32
   | SInt32 -> to_int32
   | Fixed32 -> to_int32
   | SFixed32 -> to_int32
   | Int32_int -> to_int
   | UInt32_int -> to_int
   | SInt32_int -> to_int
   | Fixed32_int -> to_int
   | SFixed32_int -> to_int
   | UInt64 -> to_int64
   | Int64 -> to_int64
   | SInt64 -> to_int64
   | Fixed64 -> to_int64
   | SFixed64 -> to_int64
   | UInt64_int -> to_int
   | Int64_int -> to_int
   | SInt64_int -> to_int
   | Fixed64_int -> to_int
   | SFixed64_int -> to_int
   | Bool -> to_bool
   | String -> to_string
   | Bytes -> to_bytes
   | Enum (module Enum) -> to_enum (module Enum)
   | Message (module Message) -> Message.from_json_exn

let find_field (_number, field_name, json_name) fields =
  match FieldMap.find_opt json_name fields with
  | Some value -> Some value
  | None -> FieldMap.find_opt field_name fields

let rec read: type a. fields -> a Spec.compound -> a = fun fields -> function
  | Basic (index, spec, default) ->
    begin
      match find_field index fields with
      | Some field -> read_value spec field
      | None -> default
    end
  | Basic_opt (index, spec) ->
    begin
      match find_field index fields with
      | Some field -> Some (read_value spec field)
      | None -> None
    end
  | Basic_req (index, spec) ->
    begin
      match find_field index fields with
      | Some field -> read_value spec field
      | None -> Result.raise (`Required_field_missing (0, ""))
    end
  | Repeated (index, spec, _packed) ->
    begin
      match find_field index fields with
      | Some field ->
        let read = read_value spec in
        to_list field |> List.map ~f:read
      | None -> []
    end
  | Oneof (oneofs, _) ->
    let rec inner = function
      | Oneof_elem (index, spec, (constr, _)) :: rest ->
        begin
          match read fields (Spec.Basic_opt (index, spec)) with
          | Some v -> constr v
          | None -> inner rest
        end
      | [] -> `not_set
    in
    inner oneofs


let rec deserialize: type constr a. (constr, a) compound_list -> constr -> fields -> a = function
  | Nil -> fun constr _json -> constr
  | Nil_ext _extension_ranges -> fun constr _json -> constr [] (* TODO implement support for extensions *)
  | Cons (spec, rest) ->
    fun constr fields ->
      let v = read fields spec in
      deserialize rest (constr v) fields


let deserialize: type constr a. (constr, a) compound_list -> constr -> Yojson.Basic.t -> a =
  fun spec constr -> function
  | `Assoc fields ->
    fields
    |> List.filter ~f:(function (_, `Null) -> false | _ -> true)
    |> FieldMap.of_list
    |> deserialize spec constr
  | json -> value_error "message" json
