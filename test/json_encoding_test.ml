open Json_encoding.Json_test
module G = Google_types_pp
open StdLabels

let%expect_test _ =
  let module T = Duration in
  let module I = G.Duration.Google.Protobuf.Duration in
  Some (I.make ()) |> Test_lib.test_encode ~debug_json:true (module T);
  Some (I.make ~seconds:10 ~nanos:5 ()) |> Test_lib.test_encode ~debug_json:true (module T);
  Some (I.make ~seconds:10 ()) |> Test_lib.test_encode ~debug_json:true (module T);
  Some (I.make ~nanos:5 ()) |> Test_lib.test_encode ~debug_json:true (module T);
  ();
  [%expect {|
    duration {
    }
    Json: { "duration": "0s" }
    Ref:  { "duration": "0s" }
    duration {
      seconds: 10
      nanos: 5
    }
    Json: { "duration": "10.000000005s" }
    Ref:  { "duration": "10.000000005s" }
    duration {
      seconds: 10
    }
    Json: { "duration": "10s" }
    Ref:  { "duration": "10s" }
    duration {
      nanos: 5
    }
    Json: { "duration": "0.000000005s" }
    Ref:  { "duration": "0.000000005s" } |}]

let%expect_test _ =
  let module T = Timestamp in
  let module I = G.Timestamp.Google.Protobuf.Timestamp in
  Some (I.make ()) |> Test_lib.test_encode ~debug_json:true (module T);
  Some (I.make ~seconds:1709985346 ~nanos:5 ()) |> Test_lib.test_encode ~debug_json:true (module T);
  Some (I.make ~seconds:1709985346 ()) |> Test_lib.test_encode ~debug_json:true (module T);
  Some (I.make ~nanos:5 ()) |> Test_lib.test_encode ~debug_json:true (module T);
  ();
  [%expect {|
    timestamp {
    }
    Json: { "timestamp": "1970-01-01T00:00:00.000000000Z" }
    Ref:  { "timestamp": "1970-01-01T00:00:00Z" }
    timestamp {
      seconds: 1709985346
      nanos: 5
    }
    Json: { "timestamp": "2024-03-09T11:55:46.000000005Z" }
    Ref:  { "timestamp": "2024-03-09T11:55:46.000000005Z" }
    timestamp {
      seconds: 1709985346
    }
    Json: { "timestamp": "2024-03-09T11:55:46.000000000Z" }
    Ref:  { "timestamp": "2024-03-09T11:55:46Z" }
    timestamp {
      nanos: 5
    }
    Json: { "timestamp": "1970-01-01T00:00:00.000000005Z" }
    Ref:  { "timestamp": "1970-01-01T00:00:00.000000005Z" } |}]

let%expect_test _ =
  let module T = Empty in
  let module I = G.Empty.Google.Protobuf.Empty in
  Some (I.make ()) |> Test_lib.test_encode ~debug_json:true (module T);
  ();
  [%expect {|
    empty {
    }
    Json: { "empty": {} }
    Ref:  { "empty": {} } |}]


let%expect_test _ =
  let module T = Wrappers in
  T.make () |> Test_lib.test_encode ~debug_json:true (module T);
  let bytes = Bytes.of_string "bytes" in
  T.make ~double:0.0 ~float:0.0 ~s64:0 ~u64:0 ~s32:0 ~u32:0 ~string:"" ~bytes:Bytes.empty ~bool:false ()
  |> Test_lib.test_encode ~debug_json:true (module T);
  T.make ~double:5.5 ~float:5.5 ~s64:5 ~u64:5 ~s32:5 ~u32:5 ~string:"str" ~bytes ~bool:true ()
  |> Test_lib.test_encode ~debug_json:true (module T);
  ();
  [%expect {|
    Json: {}
    Ref:  {}
    double {
    }
    float {
    }
    s64 {
    }
    u64 {
    }
    s32 {
    }
    u32 {
    }
    bool {
    }
    string {
    }
    bytes {
    }
    Json: {
      "double": 0,
      "float": 0,
      "s64": "0",
      "u64": "0",
      "s32": 0,
      "u32": 0,
      "bool": false,
      "string": "",
      "bytes": ""
    }
    Ref:  {
      "double": 0,
      "float": 0,
      "s64": "0",
      "u64": "0",
      "s32": 0,
      "u32": 0,
      "bool": false,
      "string": "",
      "bytes": ""
    }
    double {
      value: 5.5
    }
    float {
      value: 5.5
    }
    s64 {
      value: 5
    }
    u64 {
      value: 5
    }
    s32 {
      value: 5
    }
    u32 {
      value: 5
    }
    bool {
      value: true
    }
    string {
      value: "str"
    }
    bytes {
      value: "bytes"
    }
    Json: {
      "double": 5.5,
      "float": 5.5,
      "s64": "5",
      "u64": "5",
      "s32": 5,
      "u32": 5,
      "bool": true,
      "string": "str",
      "bytes": "Ynl0ZXM="
    }
    Ref:  {
      "double": 5.5,
      "float": 5.5,
      "s64": "5",
      "u64": "5",
      "s32": 5,
      "u32": 5,
      "bool": true,
      "string": "str",
      "bytes": "Ynl0ZXM="
    } |}]

let%expect_test _ =
  let module T = FieldMask in
  T.make () |> Test_lib.test_encode ~debug_json:true (module T);
  T.make ~mask:["a"; "b"] ()
  |> Test_lib.test_encode ~debug_json:true (module T);
  let mask = ["camel_case"; "CAMEL"] in
  let t = T.make ~mask () in
  let expect = T.make ~mask:(List.map ~f:String.lowercase_ascii mask) () in
  Test_lib.test_encode ~debug_json:true ~expect (module T) t;
  ();
  [%expect {|
    Json: {}
    Ref:  {}
    mask {
      paths: "a"
      paths: "b"
    }
    Json: { "mask": "a,b" }
    Ref:  { "mask": "a,b" }
    mask {
      paths: "camel_case"
      paths: "camel"
    }
    Json: { "mask": "camelCase,camel" }
    Ref:  { "mask": "camelCase,camel" } |}]

let%expect_test _ =
  let module T = Struct in
  let module I = G.Struct.Google.Protobuf.Struct in
  let module V = G.Struct.Google.Protobuf.Value in
  T.make () |> Test_lib.test_encode ~debug_json:true (module T);

  let v b f s =
    ["Bool", `Bool_value b; "Number", `Number_value f; "String", `String_value s]
    |>  List.map ~f:(fun (k,v) -> (k, Some (G.Struct.Google.Protobuf.Value.make ~kind:v ())))
  in
  let make struct' =
    T.make ~struct' ()
  in
  make (v true 5.0 "hello") |> Test_lib.test_encode ~debug_json:true (module T);
  make (v false 0.0 "") |> Test_lib.test_encode ~debug_json:true (module T);

  (* Test embeded struct *)
  make [ "Struct1", Some (`Struct_value (v true 1.0 "a")); "Struct2", Some (`Struct_value (v false 2.0 "b")) ]
  |> Test_lib.test_encode ~debug_json:true (module T);

  make [ "List", Some (`List_value (List.map ~f:(fun v -> V.make ~kind:(`Number_value v) ()) [3.;4.;5.])) ]
  |> Test_lib.test_encode ~debug_json:true (module T);

  ();
  [%expect {|
    Json: {}
    Ref:  {}
    struct {
      fields {
        key: "Bool"
        value {
          bool_value: true
        }
      }
      fields {
        key: "Number"
        value {
          number_value: 5
        }
      }
      fields {
        key: "String"
        value {
          string_value: "hello"
        }
      }
    }
    Json: { "struct": { "Bool": true, "Number": 5, "String": "hello" } }
    Ref:  { "struct": { "Bool": true, "Number": 5, "String": "hello" } }
    struct {
      fields {
        key: "Bool"
        value {
          bool_value: false
        }
      }
      fields {
        key: "Number"
        value {
          number_value: 0
        }
      }
      fields {
        key: "String"
        value {
          string_value: ""
        }
      }
    }
    Json: { "struct": { "Bool": false, "Number": 0, "String": "" } }
    Ref:  { "struct": { "Bool": false, "Number": 0, "String": "" } }
    struct {
      fields {
        key: "Struct1"
        value {
          struct_value {
            fields {
              key: "Bool"
              value {
                bool_value: true
              }
            }
            fields {
              key: "Number"
              value {
                number_value: 1
              }
            }
            fields {
              key: "String"
              value {
                string_value: "a"
              }
            }
          }
        }
      }
      fields {
        key: "Struct2"
        value {
          struct_value {
            fields {
              key: "Bool"
              value {
                bool_value: false
              }
            }
            fields {
              key: "Number"
              value {
                number_value: 2
              }
            }
            fields {
              key: "String"
              value {
                string_value: "b"
              }
            }
          }
        }
      }
    }
    Json: {
      "struct": {
        "Struct1": { "Bool": true, "Number": 1, "String": "a" },
        "Struct2": { "Bool": false, "Number": 2, "String": "b" }
      }
    }
    Ref:  {
      "struct": {
        "Struct1": { "Bool": true, "Number": 1, "String": "a" },
        "Struct2": { "Bool": false, "Number": 2, "String": "b" }
      }
    }
    struct {
      fields {
        key: "List"
        value {
          list_value {
            values {
              number_value: 3
            }
            values {
              number_value: 4
            }
            values {
              number_value: 5
            }
          }
        }
      }
    }
    Json: { "struct": { "List": [ 3, 4, 5 ] } }
    Ref:  { "struct": { "List": [ 3, 4, 5 ] } } |}]
