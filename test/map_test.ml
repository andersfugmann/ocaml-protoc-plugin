open Map

let proto_file = "map.proto"

let%expect_test _ =
  let module T = Map.Test in
  let t = [ 2, "2"; 3, "3"; 1, "1"; 4, "4" ] in
  Test_lib.test_encode ~skip_json:true ~proto_file (module T) t;
  [%expect {|
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
      value: "4"
    } |}]

let%expect_test _ =
  let module T = Map.Bool_map in
  let t = [ true, "true"; false, "false" ] in
  Test_lib.test_encode ~skip_json:true ~skip_protoc:true ~proto_file (module T) t;
  [%expect {| |}]



let%expect_test _ =
  let module T = Map.Two in
  let t = T.{ m = [ 1, "10"; 2, "1"; 3, "2"; 4, "3" ];
              n = [ 1, 1.0; 2, 2.0; 3, 3.0; 4, 4.0 ]} in
  Test_lib.test_encode ~proto_file (module T) t;

  [%expect {|
    m {
      key: 1
      value: "10"
    }
    m {
      key: 2
      value: "1"
    }
    m {
      key: 3
      value: "2"
    }
    m {
      key: 4
      value: "3"
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
    n {
      key: 4
      value: 4
    } |}]


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
  (* Skip protoc and json tests due to a bug where neither keys or values in a map can be null or the default value *)
  Test_lib.test_encode ~skip_protoc:true ~skip_json:true ~proto_file (module T) t;

  [%expect {| |}]
