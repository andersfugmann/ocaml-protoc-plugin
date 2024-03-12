open Package
let proto_file = "package.proto"

let%expect_test _ =
  let module T = Package.A.B.M in
  let t = 7 in
  Test_lib.test_encode ~proto_file (module T) t;
  [%expect {|
    i: 7 |}]
