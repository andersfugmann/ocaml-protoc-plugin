module Call = Service_empty_package.Test.Call

let%expect_test "service attributes" =
  Printf.printf "name: %s\n" Call.name;
  Printf.printf "package_name: %s\n" (Option.value ~default:"<none>"  Call.package_name);
  Printf.printf "service_name: %s\n" Call.service_name;
  Printf.printf "method_name: %s\n" Call.method_name;
  ();
  [%expect {|
    name: /Test/Call
    package_name: <none>
    service_name: Test
    method_name: Call |}]
