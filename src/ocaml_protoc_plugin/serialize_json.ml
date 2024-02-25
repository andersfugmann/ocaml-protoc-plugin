open Spec

(** Serialize to json as per https://protobuf.dev/programming-guides/proto3/#json-options *)

type field = string * Yojson.Basic.t
(*
[ `Assoc of (string * t) list
      | `Bool of bool
      | `Float of float
      | `Int of int
      | `List of t list
      | `Null
   | `String of string ]
*)
let float_value v = match v with
  | v when Float.is_nan v -> `String "NaN"
  | v when Float.is_finite v -> `Float v
  | v when v < 0.0 -> `String "-Infinity"
  | _ -> `String "Infinity"

let int32_value v = `Int (Int32.to_int v)
let int32_int_value v = `Int v
let int64_value v = `String (Int64.to_string v)
let int64_int_value v = `String (string_of_int v)
let bool_value v = `Bool v
let enum_value ~f v = `Int (f v)

let key ~json_names (_, name, json_name) =
  match json_names with
  | true -> json_name
  | false -> name

let rec json_of_spec: type a. a spec -> a -> Yojson.Basic.t = function
  | Double -> float_value
  | Float -> float_value
  | Bool -> fun v -> `Bool v
  | String -> fun v -> `String v
  | Bytes -> fun v -> `String (Base64.encode_string ~pad:true (Bytes.unsafe_to_string v))

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

  | Enum (module Enum) -> enum_value ~f:Enum.to_int
  | Message _ -> failwith "Unsupported. We don't have the spec!"

and write: type a. enum_names:bool -> json_names:bool -> omit_default_values:bool -> a compound -> a -> field option =
  fun ~enum_names ~json_names ~omit_default_values -> ignore enum_names; function
    | Basic (index, spec, default) -> begin
        function
        | v when omit_default_values && v == default ->
          None
        | v ->
          let value = json_of_spec spec v in
          Some (key ~json_names index, value)
      end
    | _ -> failwith "Not implemented"


let rec serialize: type a. ?enum_names:bool -> ?json_names:bool -> ?omit_default_values:bool -> (a, Yojson.Basic.t) compound_list -> field list -> a =
  fun ?(enum_names=true) ?(json_names=true) ?(omit_default_values=true) -> function
  | Nil -> (fun json -> `Assoc json)
  | Nil_ext _extension_ranges ->
    failwith "Extensions not handled by json decoder"
  | Cons (compound, rest) ->
    let cont = serialize rest in
    let write = write ~enum_names ~json_names ~omit_default_values compound in
    fun acc v ->
      let acc = match write v with
        | Some v -> v :: acc
        | None -> acc
      in
      cont acc
