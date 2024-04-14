let proto_file = "empty_message.proto"
let%expect_test _ =
  let module T = Empty_message.Empty in
  let validate = T.make () in
  let t = () in
  Test_lib.test_encode (module T) ~proto_file ~validate t;
  [%expect {| |}]
