open! StdLabels
open Spec

(** Serialize to json as per https://protobuf.dev/programming-guides/proto3/#json-options *)

type field = string * Yojson.Basic.t

let float_value v = `Float v
let int32_value v = `Int (Int32.to_int v)
let int32_int_value v = `Int v
let int64_value v = `String (Int64.to_string v)
let int64_int_value v = `String (string_of_int v)
let bool_value v = `Bool v
let enum_value ~f v = `Int (f v)
let enum_name ~f v = `String (f v)
let string_value v = `String v
let bytes_value v = `String (Base64.encode_string ~pad:true (Bytes.unsafe_to_string v))
let list_value v = `List v

let key ~json_names (_, name, json_name) =
  match json_names with
  | true -> json_name
  | false -> name

let rec json_of_spec: type a. enum_names:bool -> json_names:bool -> omit_default_values:bool -> a spec -> a -> Yojson.Basic.t =
  fun ~enum_names ~json_names ~omit_default_values -> function
  | Double -> float_value
  | Float -> float_value
  | Bool -> bool_value
  | String -> string_value
  | Bytes -> bytes_value

  | Int32 -> int32_value
  | UInt32 -> int32_value
  | SInt32 -> int32_value
  | Fixed32 -> int32_value
  | SFixed32 -> int32_value

  | Int32_int -> int32_int_value
  | UInt32_int -> int32_int_value
  | SInt32_int -> int32_int_value
  | Fixed32_int -> int32_int_value
  | SFixed32_int -> int32_int_value

  | Int64 -> int64_value
  | UInt64 -> int64_value
  | SInt64 -> int64_value
  | Fixed64 -> int64_value
  | SFixed64 -> int64_value

  | Int64_int -> int64_int_value
  | UInt64_int -> int64_int_value
  | SInt64_int -> int64_int_value
  | Fixed64_int -> int64_int_value
  | SFixed64_int -> int64_int_value

  | Enum (module Enum) -> begin
      match enum_names with
      | true -> enum_name ~f:Enum.to_string
      | false -> enum_value ~f:Enum.to_int
    end
  | Message (module Message) ->
    Message.to_json ~enum_names ~json_names ~omit_default_values

and write: type a. enum_names:bool -> json_names:bool -> omit_default_values:bool -> a compound -> a -> field option =
  fun ~enum_names ~json_names ~omit_default_values -> ignore enum_names; function
    | Basic (index, spec, default) ->
      begin
        function
        | v when omit_default_values && v == default ->
          None
        | v ->
          let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec v in
          Some (key ~json_names index, value)
      end
    | Basic_opt (index, spec) ->
      begin
        function
        | Some v ->
          let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec v in
          Some (key ~json_names index, value)
        | None -> None
      end
  | Basic_req (index, spec) -> fun v ->
    let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec v in
    Some (key ~json_names index, value)
  | Repeated (index, spec, _packed) -> fun v ->
    let to_json = json_of_spec ~enum_names ~json_names ~omit_default_values spec in
    let value = List.map ~f:to_json v |> list_value in
    Some (key ~json_names index, value)
  | Oneof (oneofs, index_f) -> begin
      function
      | `not_set -> None
      | v ->
        let index = index_f v in
        let Oneof_elem (index, spec, (_, destr)) = List.nth oneofs index in
        let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec (destr v) in
        Some (key ~json_names index, value)
    end

let rec serialize: type a. enum_names:bool -> json_names:bool -> omit_default_values:bool -> (a, Yojson.Basic.t) compound_list -> field list -> a =
  fun ~enum_names ~json_names ~omit_default_values -> function
  | Nil -> (fun json -> `Assoc json)
  | Nil_ext _extension_ranges ->
    (fun json _extensions -> `Assoc json)
  | Cons (compound, rest) ->
    let cont = serialize rest in
    let write = write ~enum_names ~json_names ~omit_default_values compound in
    fun acc v ->
      let acc = match write v with
        | Some v -> v :: acc
        | None -> acc
      in
      cont ~enum_names ~json_names ~omit_default_values acc

let serialize: ?enum_names:bool -> ?json_names:bool -> ?omit_default_values:bool -> ('a, Yojson.Basic.t) compound_list -> 'a =
  fun ?(enum_names=true) ?(json_names=true) ?(omit_default_values=true) spec ->
  serialize ~enum_names ~json_names ~omit_default_values spec []
