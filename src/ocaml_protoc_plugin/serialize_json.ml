open! StdLabels
open Spec

(** Serialize to json as per https://protobuf.dev/programming-guides/proto3/#json-options *)

type field = string * Yojson.Basic.t

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
let float_value v =
  match Float.is_integer v with
  | true -> `Int (Float.to_int v)
  | false -> `Float v

let key_to_string = function
  | `String s -> s
  | `Bool b -> string_of_bool b
  | `Int v -> string_of_int v
  | json -> Result.raise (`Wrong_field_type ("map key", (Yojson.Basic.to_string json)))

let key ~json_names (_, name, json_name) =
  match json_names with
  | true -> json_name
  | false -> name

let rec json_of_spec: type a b. enum_names:bool -> json_names:bool -> omit_default_values:bool -> (a, b) spec -> a -> Yojson.Basic.t =
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
  | Message ((module Message), _) ->
    Message.to_json ~enum_names ~json_names ~omit_default_values

and write: type a b. enum_names:bool -> json_names:bool -> omit_default_values:bool -> (a, b) compound -> a -> field list =
  fun ~enum_names ~json_names ~omit_default_values -> function
    | Basic (index, spec, default) ->
      begin
        function
        | v when omit_default_values && v = default -> []
        | v ->
          let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec v in
          [key ~json_names index, value]
      end
    | Basic_opt (index, spec) ->
      begin
        function
        | Some v ->
          let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec v in
          [key ~json_names index, value]
        | None -> []
      end
  | Basic_req (index, spec) -> fun v ->
    let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec v in
    [key ~json_names index, value]
  | Repeated (index, spec, _packed) -> fun v ->
    let to_json = json_of_spec ~enum_names ~json_names ~omit_default_values spec in
    let value = List.map ~f:to_json v |> list_value in
    [key ~json_names index, value]
  | Map (index, (key_spec, value_compound)) -> fun vs ->
    let json_of_key = json_of_spec ~enum_names ~json_names ~omit_default_values:false key_spec in
    let json_of_value = match value_compound with
      | Basic (_, spec, _) -> json_of_spec ~enum_names ~json_names ~omit_default_values spec
      | Basic_opt (_, spec) ->
        let json_of_value = json_of_spec ~enum_names ~json_names ~omit_default_values spec in
        let json_of_value = function
          | None -> `Null
          | Some v -> json_of_value v
        in
        json_of_value
    in
    let json_of_entry (key, value) =
      let key = json_of_key key |> key_to_string in
      let value = json_of_value value in
      (key, value)
    in
    begin
      match vs with
      | [] when not omit_default_values -> []
      | vs -> [key ~json_names index, `Assoc (List.map ~f:json_of_entry vs )]
    end
  | Oneof (oneofs, index_f) -> begin
      function
      | `not_set -> []
      | v ->
        let index = index_f v in
        let Oneof_elem (index, spec, (_, destr)) = List.nth oneofs index in
        let value = json_of_spec ~enum_names ~json_names ~omit_default_values spec (destr v) in
        [key ~json_names index, value]
    end

let rec serialize: type a. enum_names:bool -> json_names:bool -> omit_default_values:bool -> (a, Yojson.Basic.t) compound_list -> field list -> a =
  fun ~enum_names ~json_names ~omit_default_values -> function
  | Nil -> (fun json -> `Assoc (List.rev json))
  | Nil_ext _extension_ranges ->
    (fun json _extensions -> `Assoc (List.rev json))
  | Cons (compound, rest) ->
    let cont = serialize rest in
    let write = write ~enum_names ~json_names ~omit_default_values compound in
    fun acc v ->
     let v = write v in
     cont ~enum_names ~json_names ~omit_default_values (List.rev_append v acc)

let serialize: ?enum_names:bool -> ?json_names:bool -> ?omit_default_values:bool -> ('a, Yojson.Basic.t) compound_list -> 'a =
  fun ?(enum_names=true) ?(json_names=true) ?(omit_default_values=true) spec ->
  serialize ~enum_names ~json_names ~omit_default_values spec []
