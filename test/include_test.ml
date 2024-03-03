open Include
module Enum = Enum.Enum_test
let test_encode = Test_lib.test_encode ~skip_json:true
let%expect_test _ =
  let module T = Include.I in
  let t = T.{ enum = Enum.Message.E.B;
              m = Some 3;
              o = Some Enum.E1.C;
              c = Some 7;
            } in
  test_encode (module T) t;
  [%expect {|
    enum: B
    m {
      i: 3
    }
    o {
      enum: C
    }
    c {
      i: 7
    } |}]


let%expect_test _ =
  let module T = Include.Z in
  let t = Some Included.Include.N.E.B  in
  test_encode (module T) t;
  [%expect {|
    n {
      e: B
    } |}]

let%expect_test _ =
  let module T = Include.Y in
  let t = Some 42 in
  test_encode (module T) t;
  [%expect {|
    d {
      i: 42
    } |}]
