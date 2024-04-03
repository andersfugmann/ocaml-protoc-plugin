let proto_file = "recursive.proto"

let _ =
  let _x = Recursive2.X.make () in
  let _x' = { _x with a = Some _x } in (* Wrong *)
  let _x_x = Recursive2.X.X.make () in
  let _x_x' = { _x_x with x = Some _x_x } in
  ()
