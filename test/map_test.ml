open Map

let proto_file = "map.proto"

let%expect_test _ =
  let module T = Map.Test in
  let t = [ 1, "1"; 2, "2"; 3, "3"; 0, "1"; 4, "" ] in
  Test_lib.test_encode ~skip_json:true ~proto_file (module T) t;
  [%expect {|
    m {
      key: 0
      value: "1"
    }
    m {
      key: 1
      value: "1"
    }
    m {
      key: 2
      value: "2"
    }
    m {
      key: 3
      value: "3"
    }
    m {
      key: 4
      value: ""
    } |}]

let%expect_test _ =
  let module T = Map.Bool_map in
  let t = [ true, "true"; false, "false" ] in
  Test_lib.test_encode ~proto_file (module T) t;
  [%expect {|
    m {
      key: false
      value: "false"
    }
    m {
      key: true
      value: "true"
    } |}]



let%expect_test _ =
  let module T = Map.Two in
  let t = T.{ m = [ 0, "10"; 1, "1"; 2, "2"; 3, "3" ];
              n = [ 0, 0.0; 1, 1.0; 2, 2.0; 3, 3.0 ]} in
  Test_lib.test_encode ~proto_file (module T) t;
  Printf.printf "The reference implementation drops elements when the values is the default value";

  [%expect {|
    m {
      key: 0
      value: "10"
    }
    m {
      key: 1
      value: "1"
    }
    m {
      key: 2
      value: "2"
    }
    m {
      key: 3
      value: "3"
    }
    n {
      key: 0
      value: 0
    }
    n {
      key: 1
      value: 1
    }
    n {
      key: 2
      value: 2
    }
    n {
      key: 3
      value: 3
    }
    Cannot deserialize reference json.
    Generated json not equal
    Json: {
      "m": { "0": "10", "1": "1", "2": "2", "3": "3" },
      "n": { "0": 0, "1": 1, "2": 2, "3": 3 }
    }
    Ref:  {
      "m": { "0": "10", "1": "1", "2": "2", "3": "3" },
      "n": { "1": 1, "2": 2, "3": 3 }
    }
    The reference implementation drops elements when the values is the default value |}]


let%expect_test _ =
  let module T = Map.Map_message in
  let t = [ 1, Some 1;
            2, Some 2;
            3, Some 3;
            4, Some 4; ]
  in
  Test_lib.test_encode ~proto_file (module T) t;
  [%expect {|
    m {
      key: 1
      value {
        i: 1
      }
    }
    m {
      key: 2
      value {
        i: 2
      }
    }
    m {
      key: 3
      value {
        i: 3
      }
    }
    m {
      key: 4
      value {
        i: 4
      }
    } |}]

let%expect_test _ =
  let module T = Map.Map_message in
  let t = [ 0, Some 0;
            1, Some 1;
            2, Some 2;
            3, None;
            4, Some 4;
            10, Some 0;
          ]
  in
  Test_lib.test_encode ~proto_file (module T) t;
  Printf.printf "The reference implementation drops elements when the values is the default value";

  [%expect {|
    m {
      key: 0
      value {
      }
    }
    m {
      key: 1
      value {
        i: 1
      }
    }
    m {
      key: 2
      value {
        i: 2
      }
    }
    m {
      key: 3
      value {
      }
    }
    m {
      key: 4
      value {
        i: 4
      }
    }
    m {
      key: 10
      value {
      }
    }
    Cannot deserialize reference json.
    Generated json not equal
    Json: {
      "m": {
        "0": {},
        "1": { "i": "1" },
        "2": { "i": "2" },
        "3": null,
        "4": { "i": "4" },
        "10": {}
      }
    }
    Ref:  {
      "m": {
        "0": {},
        "1": { "i": "1" },
        "2": { "i": "2" },
        "4": { "i": "4" },
        "10": {}
      }
    }
    The reference implementation drops elements when the values is the default value |}]
