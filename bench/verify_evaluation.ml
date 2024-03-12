module M = Verify.Verify.M
module N = Verify.Verify.N

(* We want to construct a large structure and then call serialization and deserialization 1000 times to understand how many times the serialization function are being evaluation *)

let m =
  M.make ~a:"Test" ~b:5 ~rp:[1;2;3] ~r:[1;2;3]
    ~m:[true, "true"; false, "false"] ~message:(N.make ~i:7 ())
    ~enum:Verify.Verify.E.B
    ~oneof:(`J "hello") ()

let _ =
  let to_json = M.to_json Ocaml_protoc_plugin.Json_options.default in
  for _ = 1 to 2579-1 do
    M.to_proto m |> Sys.opaque_identity |> ignore;
    to_json m |> Sys.opaque_identity |> ignore;
    ()
  done;
  let reader =
    M.to_proto m
    |> Ocaml_protoc_plugin.Writer.contents
    |> Ocaml_protoc_plugin.Reader.create
  in
  let json = to_json m in
  for _ = 1 to 4177 do
    M.from_proto_exn reader |> Sys.opaque_identity |> ignore;
    M.from_json_exn json |> Sys.opaque_identity |> ignore;
    ()
  done;


  ()
