open Large

let proto_file = "large.proto"

let%expect_test "Test very large message type" =
  let large = Large.make ~x7:7 () in
  Test_lib.test_encode ~proto_file (module Large) large;
  ();
  [%expect {|
    x7: 7 |}]
