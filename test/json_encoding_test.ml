open Json_encoding
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
