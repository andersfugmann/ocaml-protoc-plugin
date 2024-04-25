open StdLabels
open Int_types

let proto_file = "int_types.proto"

let test_signed (type t) ~(create : int -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name ());
  let values = [-1073741823; -2; -1; 0; 1; 2; 1073741823] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode ~skip_json:true ~proto_file (module T) (create v))
    values

let test_unsigned (type t) ~(create : int -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name ());
  let values = [0; 1; 2; 2147483647; 4294967295] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode ~proto_file (module T) (create v))
    values

let%expect_test _ =
  let module T = Int_types.SInt64 in
  let create i = i in
  test_signed ~create (module T);
  [%expect {|
    Test .int_types.SInt64
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.SInt32 in
  let create i = i in
  test_signed ~create (module T);
  [%expect {|
    Test .int_types.SInt32
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.Int64 in
  let create i = i in
  test_signed ~create (module T);
  [%expect {|
    Test .int_types.Int64
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.Int32 in
  let create i = i in
  test_signed ~create (module T);
  [%expect
    {|
    Test .int_types.Int32
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.UInt64 in
  let create i = i in
  test_unsigned ~create (module T);
  [%expect {|
    Test .int_types.UInt64
    i: 1
    i: 2
    i: 2147483647
    i: 4294967295 |}]

let%expect_test _ =
  let module T = Int_types.UInt32 in
  let create i = i in
  test_unsigned ~create (module T);
  [%expect {|
    Test .int_types.UInt32
    i: 1
    i: 2
    i: 2147483647
    i: 4294967295 |}]
