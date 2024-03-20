open! StdLabels
open Spec

(** Serialize to json as per https://protobuf.dev/programming-guides/proto3/#json-options *)
let value_error type_name json =
  Result.raise (`Wrong_field_type (type_name, Json.to_string json))

type field = string * Json.t

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
  | json -> Result.raise (`Wrong_field_type ("map key", (Json.to_string json)))

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
  let open StdLabels in
  String.split_on_char ~sep:'_' s
  |> List.map ~f:String.lowercase_ascii
  |> List.map ~f:String.capitalize_ascii
  |> String.concat ~sep:""
  |> String.uncapitalize_ascii

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
    Camel_case ->  camelCase
    Camel_Case ->  camelCase
    Camel_Case ->  camelCase
    camel_cASe ->  camelCase
    CAMel_case ->  camelCase |}]

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
    Printf.printf "%d.%d -> %s\n" seconds nanos (Json.to_string (duration_to_json json))
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
    Printf.printf "%d.%d -> %s\n" seconds nanos (Json.to_string (timestamp_to_json json))
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

let map_enum_json: (module Enum) -> Json.t -> Json.t = fun (module Enum) ->
  let name =
    Enum.name ()
    |> String.split_on_char ~sep:'.'
    |> List.tl
    |> String.concat ~sep:"."
  in
  match name with
  | "google.protobuf.NullValue" -> begin
      function
      | `Int 0 -> `Null
      | `String s when s = Enum.to_string (Enum.from_int_exn 0) -> `Null
      | json -> value_error name json
    end
  | _ -> fun json -> json


(* Convert already emitted json based on json mappings *)
let map_message_json: name:string -> (Json.t -> Json.t) option = fun ~name ->
  match name with
  | ".google.protobuf.Empty"  ->
    Some (fun json -> json)
  (* Duration - google/protobuf/timestamp.proto *)
  | ".google.protobuf.Duration" ->
    Some (duration_to_json)
  (* Timestamp - google/protobuf/timestamp.proto *)
  | ".google.protobuf.Timestamp" ->
    Some (timestamp_to_json)
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
    Some (wrapper_to_json)
  | ".google.protobuf.Value" ->
    let map = function
      | `Assoc [_, json] -> json
      | json -> value_error name json
    in
    Some map
  | ".google.protobuf.Struct" ->
    let map = function
      | `Assoc ["fields", json ] -> json
      | json -> value_error name json
    in
    Some map
  | ".google.protobuf.ListValue" ->
    let map = function
      | `Assoc ["values", json ] -> json
      | json -> value_error name json
    in
    Some map
  | ".google.protobuf.FieldMask" ->
    let open StdLabels in
    let map = function
      | `Assoc ["paths", `List masks] ->
        List.map ~f:(function
          | `String mask -> (to_camel_case mask)
          | json -> value_error name json
        ) masks
        |> String.concat ~sep:","
        |> fun mask -> `String mask
      | json -> value_error name json
    in
    Some map
  | _ -> None

let rec json_of_spec: type a b. Json_options.t -> (a, b) spec -> a -> Json.t =
  fun options -> function
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
      let map_enum_json = map_enum_json (module Enum) in
      let f = match options.enum_names with
        | true -> enum_name ~f:Enum.to_string
        | false -> enum_value ~f:Enum.to_int
      in
      fun v ->
        f v |> map_enum_json
  end
  | Message (module Message) ->
    fun v -> Message.to_json options v

and write: type a b. Json_options.t -> (a, b) compound -> a -> field list = fun options ->
  function
  | Basic (index, spec, default) -> begin
      let key = key ~json_names:options.json_names index in
      let to_json = json_of_spec options spec in
      function
      | v when options.omit_default_values && v = default -> []
      | v -> [key, to_json v]
    end
  | Basic_opt (index, spec) -> begin
      let key = key ~json_names:options.json_names index in
      let to_json = json_of_spec options spec in
      function
      | Some v ->
        [key, to_json v]
      | None -> []
    end
  | Basic_req (index, spec) ->
    let key = key ~json_names:options.json_names index in
    let to_json = json_of_spec options spec in
    fun v -> [key, to_json v]
  | Repeated (index, spec, _packed) ->
    let key = key ~json_names:options.json_names index in
    let to_json = json_of_spec options spec in
    fun v ->
      let value = List.map ~f:to_json v |> list_value in
      [key, value]
  | Map (index, (key_spec, value_compound)) ->
    let key = key ~json_names:options.json_names index in
    let json_of_key = json_of_spec { options with omit_default_values=false } key_spec in
    let json_of_value = match value_compound with
      | Basic (_, spec, _) -> json_of_spec options spec
      | Basic_opt (_, spec) ->
        let json_of_value = json_of_spec options spec in (* 3 *) (* This is a message. It will be evaluated recursively,  Indefinatly. *)
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
      function
      | [] when not options.omit_default_values -> []
      | vs -> [key, `Assoc (List.map ~f:json_of_entry vs )]
    end
  (* Something is recursive. a -> b -> a -> b -> a -> b. How do we break the chain? *)
  | Oneof (oneofs, index_f) -> begin
      let array =
        List.map ~f:(fun (Oneof_elem (index, spec, (_, destr))) -> (* 1 *)
          let to_json = json_of_spec options spec in (* 5 *)
          let key = key ~json_names:options.json_names index in
          fun v -> [key, to_json (destr v)]
        ) oneofs
        |> Array.of_list
      in
      function
      | `not_set -> []
      | v ->
        let f = array.(index_f v) in
        f v
    end

let serialize: type a. message_name:string -> Json_options.t -> (a, Json.t) compound_list -> field list -> a = fun ~message_name options ->
  let omit_default_values, map_result = match map_message_json ~name:message_name with
    | Some mapping -> false, fun json -> `Assoc (List.rev json) |> mapping
    | None -> options.omit_default_values, fun json -> `Assoc (List.rev json)
  in
  let options = { options with omit_default_values } in
  let rec inner: type a. (a, Json.t) compound_list -> field list -> a =
    function
    | Nil -> map_result
    | Nil_ext _extension_ranges -> fun json _extensions -> map_result json
    | Cons (compound, rest) ->
      let cont = inner rest in
      let write = write options compound in (* 2 *) (* 4 *)
      fun acc v ->
        let v = write v in
        cont (List.rev_append v acc)
  in
  inner

let serialize: type a. message_name:string -> (a, Json.t) compound_list -> Json_options.t -> a =
  fun ~message_name spec ->
  let arr = Array.init (Json_options.max_int + 1) ~f:(fun i -> Lazy.from_fun (fun () -> serialize ~message_name (Json_options.of_int i) spec [])) in
  fun options ->
    Lazy.force arr.(Json_options.to_int options)
