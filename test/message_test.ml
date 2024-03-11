open Message

let proto_file = "message.proto"

let%expect_test _ =
  let module T = Message.Message in
  let submessage = 3 in
  let validate = T.make ~m:submessage () in
  let t = Some submessage in
  Test_lib.test_encode ~proto_file (module T) ~validate t;
  [%expect {|
    m {
      i: 3
    } |}]

(** The message containing a submessage with all default values.
    The length of the submessage is 0, so
    the message will be index 1, with length delimited type (2):
    1 * 8 + 2 = 0xa
    The length of the delimited type is 0, so the complete message shoud be:
    0xa 0x0.
*)

let%expect_test _ =
  let module T = Message.Message in
  let validate = T.make ~m:0 () in
  let t = Some 0 in
  Test_lib.test_encode ~proto_file (module T) ~validate t;
  [%expect {|
    m {
    } |}]

let%expect_test _ =
  let module T = Message.Message in
  let validate = T.make ~m:1 () in
  let t = Some 1 in
  Test_lib.test_encode ~proto_file (module T) ~validate t;
  [%expect {|
    m {
      i: 1
    } |}]

let%expect_test _ =
  let module T = Message.Message in
  let validate = T.make () in
  let t = None in
  Test_lib.test_encode ~proto_file (module T) ~validate t;
  [%expect {| |}]

let%expect_test _ =
  let module T = Message.Message2 in
  let validate = T.make ~i:2 () in
  let t = T.{i = 2; m = None} in
  Test_lib.test_encode ~proto_file (module T) ~validate t;
  [%expect {|
    i: 2 |}]

let%expect_test _ =
  let module T = Message.Message2 in
  let submessage = 0 in
  let validate = T.make ~i:2 ~m:submessage () in
  let t = T.{i = 2; m = Some submessage} in
  Test_lib.test_encode ~proto_file (module T) ~validate t;
  [%expect {|
    i: 2
    m {
    } |}]
