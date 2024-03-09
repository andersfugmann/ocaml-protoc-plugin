open! StdLabels
open Spec

(** Serialize to json as per https://protobuf.dev/programming-guides/proto3/#json-options *)
let value_error type_name json =
  Result.raise (`Wrong_field_type (type_name, Yojson.Basic.to_string json))

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

let get_key ~f ~default key = function
  | `Assoc l ->
    List.assoc_opt key l
    |> Option.map f
    |> Option.value ~default
  | json -> value_error "Expected Assoc" json

let to_camel_case s =
  let open Stdlib in
  let is_ascii c = Char.lowercase_ascii c <> Char.uppercase_ascii c in
  let rec map = function
    | '_' :: c :: cs when is_ascii c ->
      Char.uppercase_ascii c :: map cs
    | c :: cs -> c :: map cs
    | [] -> []
  in
  String.to_seq s
  |> List.of_seq
  |> map
  |> List.to_seq
  |> String.of_seq

let%expect_test "json name to proto name" =
  let test s = Printf.printf "%10s -> %10s\n" s (to_camel_case s) in
  test "camel_case";
  test "Camel_case";
  test "Camel_Case";
  test "Camel_Case";
  test "camel_cASe";
  test "CAMel_case";
  ();
  [%expect {|
    camel_case ->  camelCase
    Camel_case ->  CamelCase
    Camel_Case ->  CamelCase
    Camel_Case ->  CamelCase
    camel_cASe ->  camelCASe
    CAMel_case ->  CAMelCase |}]

let duration_to_json json =
  let seconds = get_key "seconds" ~f:Deserialize_json.to_int64 ~default:0L json in
  let nanos = get_key "nanos" ~f:Deserialize_json.to_int32 ~default:0l json in
  let seconds = match seconds < 0L || nanos < 0l with
    | true -> Int64.mul (-1L) (Int64.abs seconds)
    | false -> (Int64.abs seconds)
  in
  let duration =
    match nanos with
    | 0l -> Printf.sprintf "%Lds" seconds
    | _ -> Printf.sprintf "%Ld.%09lds" seconds (Int32.abs nanos)
  in
  `String duration

let%expect_test "duration_to_json" =
  let test seconds nanos =
    let json = `Assoc ["seconds", `Int seconds; "nanos", `Int nanos] in
    Printf.printf "%d.%d -> %s\n" seconds nanos (Yojson.Basic.to_string (duration_to_json json))
  in
  test 100 0;
  test (1000) (123456);
  test (-1000) (-123456);
  ();
  [%expect {|
    100.0 -> "100s"
    1000.123456 -> "1000.000123456s"
    -1000.-123456 -> "-1000.000123456s" |}]

let timestamp_to_json json =
  let open Stdlib in
  let open StdLabels in
  let seconds = get_key "seconds" ~f:Deserialize_json.to_int ~default:0 json in
  let nanos = get_key "nanos" ~f:Deserialize_json.to_int ~default:0 json in
  let s1 = Ptime.Span.of_int_s seconds in
  let s2 = Ptime.Span.of_float_s (float nanos /. 1_000_000_000.0) |> Option.get in
  let t =
    Ptime.Span.add s1 s2
    |> Ptime.of_span
    |> Option.get
   in
  t
  |> Ptime.to_rfc3339 ~frac_s:9
  |> String.split_on_char ~sep:'-'
  |> List.rev
  |> List.tl
  |> List.rev
  |> String.concat ~sep:"-"
  |> fun s -> `String (s^"Z")

let%expect_test "timestamp_to_json" =
  let test seconds nanos =
    let json = `Assoc ["seconds", `Int seconds; "nanos", `Int nanos] in
    Printf.printf "%d.%d -> %s\n" seconds nanos (Yojson.Basic.to_string (timestamp_to_json json))
  in
  test 1709931283 0;
  test 1709931283 (1_000_000_002/2);
  test 1709931283 1_000_000_000;
  test 0 1;
  ();
  [%expect {|
    1709931283.0 -> "2024-03-08T20:54:43.000000000Z"
    1709931283.500000001 -> "2024-03-08T20:54:43.500000001Z"
    1709931283.1000000000 -> "2024-03-08T20:54:44.000000000Z"
    0.1 -> "1970-01-01T00:00:00.000000001Z" |}]

let wrapper_to_json json = get_key ~f:(fun id -> id) ~default:`Null "value" json

(* Convert already emitted json based on json mappings *)
let map_json: type a. (module Message with type t = a) -> (Yojson.Basic.t -> Yojson.Basic.t) option = fun (module Message) ->
  match Message.name' () |> String.split_on_char ~sep:'.' with
  | [_; "google"; "protobuf"; "Empty"]  ->
    Some (fun json -> json)
  (* Duration - google/protobuf/timestamp.proto *)
  | [_; "google"; "protobuf"; "Duration"] ->
    Some (duration_to_json)
  (* Timestamp - google/protobuf/timestamp.proto *)
  | [_; "google"; "protobuf"; "Timestamp"] ->
    Some (timestamp_to_json)
  (* Wrapper types - google/protobuf/wrappers.proto *)
  | [_; "google"; "protobuf"; "DoubleValue"]
  | [_; "google"; "protobuf"; "FloatValue"]
  | [_; "google"; "protobuf"; "Int64Value"]
  | [_; "google"; "protobuf"; "UInt64Value"]
  | [_; "google"; "protobuf"; "Int32Value"]
  | [_; "google"; "protobuf"; "UInt32Value"]
  | [_; "google"; "protobuf"; "BoolValue"]
  | [_; "google"; "protobuf"; "StringValue"]
  | [_; "google"; "protobuf"; "BytesValue"] ->
    Some (wrapper_to_json)
  | [_; "google"; "protobuf"; "Value"] ->
    let map = function
      | `Assoc [_, json] -> json
      | json -> value_error "google.protobuf.Value" json
    in
    Some map
  (* FieldMask - /usr/include/google/protobuf/field_mask.proto *)
  | [_; "google"; "protobuf"; "FieldMask"] ->
    let open StdLabels in
    let map = function
      | `Assoc ["masks", `List masks] ->
        List.map ~f:(function
          | `String mask -> (to_camel_case mask)
          | json -> value_error "google.protobuf.FieldMask" json
        ) masks
        |> String.concat ~sep:","
        |> fun mask -> `String mask
      | json -> value_error "google.protobuf.FieldMask" json
    in
    Some map
  | _ -> None

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
  | Message (module Message) ->
    let omit_default_values, map =
      match map_json (module Message) with
      | Some map -> false, map
      | None -> omit_default_values, (fun json -> json)
    in
    fun v -> Message.to_json ~enum_names ~json_names ~omit_default_values v |> map

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
