open Json_encoding.Json_test
module G = Google_types_pp

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
