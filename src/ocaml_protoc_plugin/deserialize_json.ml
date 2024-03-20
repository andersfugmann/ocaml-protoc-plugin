(** Module for deserializing values *)
open! StdLabels
open Spec

module FieldMap = Map.Make(String)
type fields = Json.t FieldMap.t

let value_error type_name json =
  Result.raise (`Wrong_field_type (type_name, Json.to_string json))

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
let to_enum: type a. (module Spec.Enum with type t = a) -> Json.t -> a = fun (module Enum) -> function
  | `String enum -> Enum.from_string_exn enum
  | `Int enum -> Enum.from_int_exn enum
  | json -> value_error "enum" json

let to_float = function
  | `Float f -> f
  | `Int i -> Float.of_int i
  | `String s -> Float.of_string s
  | json -> value_error "float" json

let to_bool = function
  | `Bool b -> b
  | `String "true" -> true
  | `String "false" -> false
  | json -> value_error "bool" json

let to_list = function
  | `List l -> l
  | json -> value_error "list" json

let read_map: type a b. read_key:(string -> a) -> read_value:(Json.t -> b) -> Json.t -> (a * b) list =
  fun ~read_key ~read_value -> function
    | `Assoc entries ->
      List.map ~f:( fun (key, value) ->
        let key = read_key key in
        let value = read_value value in
        (key, value)
      ) entries
    | json -> value_error "map_entry" json

(** What a strange encoding.
    Durations less than one second are represented with a 0 seconds field and a positive or negative nanos field. For durations of one second or more, a non-zero value for the nanos field must be of the same sign as the seconds field.
*)
let duration_of_json json =
  (* Capture the signedness of the duration, and apply to both seconds and nanos *)
  (* nanos can be 0, 3, 6 or 9 digits *)
  (* must end with a 's' *)

  try
    let s = match json with `String s -> s | json -> value_error "duration" json in
    assert (String.get s (String.length s - 1) = 's');
    let sign, s = match String.get s 0 = '-' with
      | true -> (-1), String.sub s ~pos:1 ~len:(String.length s - 2)
      | false -> 1, String.sub s ~pos:0 ~len:(String.length s - 1)
    in
    let seconds, nanos = match String.split_on_char ~sep:'.' s with
      | [seconds; nanos] -> seconds, nanos
      | [seconds] -> seconds, "000"
      | _ -> failwith "Too many '.' in string"
    in
    let seconds = int_of_string seconds in
    let nano_fac = match String.length nanos with
      | 3 -> 1_000_000
      | 6 -> 1000
      | 9 -> 1
      | _ -> failwith "Nanos should be either 0, 3, 6 or 9 digits"
    in

    let nanos = int_of_string nanos * nano_fac in
    assert (seconds >= 0 && nanos >= 0);
    (seconds * sign, nanos * sign)
  with
  | _ -> value_error "google.protobuf.duration" json

let%expect_test "google.protobuf.Duration" =
  let test str =
    try
      let (s,n) = duration_of_json (`String str) in
      Printf.printf "D: %s -> %d.%09ds\n" str s n
    with
    | Result.Error e -> Printf.printf "Error parsing %s: %s\n" str (Result.show_error e)
  in
  test "1s";
  test "-1s";
  test "1.001s";
  test "-1.001s";
  test "1.1";
  test "1.-10s";
  ();
  [%expect {|
    D: 1s -> 1.000000000s
    D: -1s -> -1.000000000s
    D: 1.001s -> 1.001000000s
    D: -1.001s -> -1.-01000000s
    Error parsing 1.1: `Wrong_field_type (("google.protobuf.duration", "\"1.1\""))
    Error parsing 1.-10s: `Wrong_field_type (("google.protobuf.duration", "\"1.-10s\"")) |}]

let timestamp_of_json = function
  | `String timestamp ->
    let t =
      match Ptime.of_rfc3339 timestamp with
      | Ok (t, _, _) -> t
      | Error _e -> value_error "google.protobuf.duration" (`String timestamp)
    in
    let seconds = Ptime.to_float_s t |> Float.to_int in
    let nanos = Ptime.frac_s t |> Ptime.Span.to_float_s |> Float.mul 1_000_000_000.0 |> Float.to_int in
    (seconds, nanos)
  | json -> value_error "google.protobuf.timestamp" json

let%expect_test "google.protobuf.Timestamp" =
  let test str =
    try
      let (s, n) = timestamp_of_json (`String str) in
      Printf.printf "D: %s -> %d.%09ds\n" str s n
    with
    | Result.Error e -> Printf.printf "Error parsing %s: %s\n" str (Result.show_error e)
  in
  test "1972-01-01T10:00:20.021Z";
  test "2024-03-06T21:10:27.020Z";
  test "2024-03-06T21:10:27.123123123Z";
  test "2024-03-06T21:10:27.999999Z";
  test "2024-03-06T21:10:27.999999";
  ();
  [%expect {|
    D: 1972-01-01T10:00:20.021Z -> 63108020.021000000s
    D: 2024-03-06T21:10:27.020Z -> 1709759427.020000000s
    D: 2024-03-06T21:10:27.123123123Z -> 1709759427.123123123s
    D: 2024-03-06T21:10:27.999999Z -> 1709759427.999999000s
    Error parsing 2024-03-06T21:10:27.999999: `Wrong_field_type (("google.protobuf.duration",
                        "\"2024-03-06T21:10:27.999999\"")) |}]

let from_camel_case s =
  let open Stdlib in
  let is_lowercase c = Char.lowercase_ascii c = c &&  Char.uppercase_ascii c <> c in
  let is_uppercase c = Char.lowercase_ascii c <> c &&  Char.uppercase_ascii c = c in
  let rec map = function
    | c1 :: c2 :: cs when is_lowercase c1 && is_uppercase c2 ->
      c1 :: '_' :: (Char.lowercase_ascii c2) :: map cs
    | c :: cs -> c :: map cs
    | [] -> []
  in
  String.to_seq s
  |> List.of_seq
  |> map
  |> List.to_seq
  |> String.of_seq

let%expect_test "json name to proto name" =
  let test s = Printf.printf "%10s -> %10s\n" s (from_camel_case s) in
  test "camelCase";
  test "CamelCase";
  test "Camel_Case";
  test "Camel_Case";
  test "camelCASe";
  test "CAMelCase";
  ();
  [%expect {|
     camelCase -> camel_case
     CamelCase -> Camel_case
    Camel_Case -> Camel_Case
    Camel_Case -> Camel_Case
     camelCASe -> camel_cASe
     CAMelCase -> CAMel_case |}]

let value_to_json json =
  let value = match json with
    | `Null -> "nullValue", json
    | `Float _ -> "numberValue", json
    | `Int _ -> "numberValue", json
    | `String _ -> "stringValue", json
    | `Bool _ -> "boolValue", json
    | `Assoc _ -> "structValue", json
    | `List _ -> "listValue", json
  in
  `Assoc [value]


let map_enum_json: (module Enum) -> Json.t -> Json.t = fun (module Enum) ->
  let name =
    Enum.name ()
    |> String.split_on_char ~sep:'.'
    |> List.tl
    |> String.concat ~sep:"."
  in
  match name with
  | "google.protobuf.NullValue" ->
    let map = function
      | `Null -> `String (Enum.to_string (Enum.from_int_exn 0))
      | json -> value_error name json
    in
    map
  | _ -> fun json -> json

let map_message_json: name:string -> Json.t -> Json.t = fun ~name ->
  match name with
  | ".google.protobuf.Empty" (* Already mapped as it should I think *) ->
    fun json -> json
  (* Duration - google/protobuf/timestamp.proto *)
  | ".google.protobuf.Duration" ->
    let convert json =
      let (seconds, nanos) = duration_of_json json in
      `Assoc [ "seconds", `Int seconds; "nanos", `Int nanos ]
    in
    convert
  (* Timestamp - google/protobuf/timestamp.proto *)
  | ".google.protobuf.Timestamp" ->
    let convert json =
      let (seconds, nanos) = timestamp_of_json json in
      `Assoc [ "seconds", `Int seconds; "nanos", `Int nanos ]
    in
    convert
  (* Wrapper types - google/protobuf/wrappers.proto *)
  | ".google.protobuf.DoubleValue"
  | ".google.protobuf.FloatValue"
  | ".google.protobuf.Int64Value"
  | ".google.protobuf.UInt64Value"
  | ".google.protobuf.Int32Value"
  | ".google.protobuf.UInt32Value"
  | ".google.protobuf.BoolValue"
  | ".google.protobuf.StringValue"
  | ".google.protobuf.BytesValue" ->
    (* Convert into a struct *)
    let convert json = `Assoc ["value", json] in
    convert
  | ".google.protobuf.Value" ->
    (* Struct - google/protobuf/struct.proto *)
    (* Based on json type do the mapping *)
    value_to_json
  | ".google.protobuf.Struct" ->
    (* Struct - google/protobuf/struct.proto *)
    let convert = function
      | `Assoc _ as json ->
        `Assoc ["fields", json ]
      | json -> value_error name json
    in
    convert
    (* ListValue - google/protobuf/struct.proto *)
  | ".google.protobuf.ListValue" ->
    let convert = function
      | `List _ as json ->  `Assoc ["values", json ]
      | json -> value_error name json
    in
    convert

  (* FieldMask - /usr/include/google/protobuf/field_mask.proto *)
  | ".google.protobuf.FieldMask" ->
    let open StdLabels in
    let convert = function
      | `String s ->
        let masks =
          String.split_on_char ~sep:',' s
          |> List.map ~f:from_camel_case
          |> List.map ~f:(fun s -> `String s)
        in
        `Assoc ["paths", `List masks]
      | json -> value_error name json
    in
    convert
  | _ -> fun json -> json

let read_value: type a b. (a, b) spec -> Json.t -> a = function
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
   | Enum (module Enum) ->
     let map_enum_json = map_enum_json (module Enum) in
     fun json -> map_enum_json json |> to_enum (module Enum)
   | Message (module Message) ->
     Message.from_json_exn

let find_field (_number, field_name, json_name) fields =
  match FieldMap.find_opt json_name fields with
  | Some value -> Some value
  | None -> FieldMap.find_opt field_name fields

let rec read: type a b. (a, b) Spec.compound -> fields -> a = function
  | Basic (index, spec, default) ->
    begin
      let read_value = read_value spec in
      fun fields ->
        match find_field index fields with
        | Some field -> read_value field
        | None -> default
    end
  | Basic_opt (index, spec) ->
    begin
      let read_value = read_value spec in
      fun fields ->
        match find_field index fields with
        | Some field -> Some (read_value field)
        | None -> None
    end
  | Basic_req (index, spec) ->
    begin
      let read_value = read_value spec in
      fun fields ->
        match find_field index fields with
        | Some field -> read_value field
        | None -> Result.raise (`Required_field_missing (0, ""))
    end
  | Repeated (index, spec, _packed) ->
    begin
      let read = read_value spec in
      fun fields ->
        match find_field index fields with
        | Some field ->
          to_list field |> List.map ~f:read
        | None -> []
    end
  | Map (index, (key_spec, Basic (_, value_spec, _))) ->
    begin
      let read_key = read_value key_spec in
      let read_key v = read_key (`String v) in
      let read_value = read_value value_spec in
      fun fields ->
        match find_field index fields with
        | Some field ->
          read_map ~read_key ~read_value field
        | None -> []
    end
  | Map (index, (key_spec, Basic_opt (_, value_spec))) ->
    begin
      let read_key = read_value key_spec in
      let read_key v = read_key (`String v) in
      let read_value = read_value value_spec in
      let read_value = function
        | `Null -> begin try Some (read_value `Null) with | _ -> None end
        | json -> Some (read_value json)
      in
      fun fields -> match find_field index fields with
        | Some field ->
          read_map ~read_key ~read_value field
        | None -> []
    end
  | Oneof (oneofs, _) ->
    begin
      let rec make_readers = function
        | Oneof_elem (index, spec, (constr, _)) :: rest ->
          let read = read (Spec.Basic_opt (index, spec)) in
          let read_opt fields =
            match read fields with
            | Some v -> Some (constr v)
            | None -> None
          in
          read_opt :: make_readers rest
        | [] -> []
      in
      let readers = make_readers oneofs in
            let rec find fields = function
        | [] -> `not_set
        | read_opt :: rest -> match read_opt fields with
          | Some v -> v
          | None -> find fields rest
      in
      fun fields -> find fields readers
    end

let rec deserialize: type constr a. (constr, a) compound_list -> constr -> fields -> a = function
  | Nil -> fun constr _json -> constr
  | Nil_ext _extension_ranges -> fun constr _json -> constr [] (* TODO: implement support for extensions *)
  | Cons (spec, rest) ->
    let read = read spec in
    let cont = deserialize rest in
    fun constr fields ->
      let v = read fields in
      cont (constr v) fields


let deserialize: type constr a. message_name:string -> (constr, a) compound_list -> constr -> Json.t -> a =
  fun ~message_name spec constr ->
  let deserialize = deserialize spec constr in
  let map_message = map_message_json ~name:message_name in
  fun json -> match map_message json with
    | `Assoc fields ->
      fields
      |> List.fold_left ~f:(fun map (key, value) -> FieldMap.add key value map) ~init:FieldMap.empty
      |> deserialize
    | json -> value_error "message" json
