open Json_enum_initialization

let%expect_test "JSON decoder initialization with a cross-message enum reference" =
  let decoded = A.from_json_exn (`Assoc ["kind", `String "ZERO"]) in
  let expected = A.make ~kind:B.Kind.ZERO () in
  Printf.printf "%b\n" (A.equal decoded expected);
  [%expect {| true |}]
