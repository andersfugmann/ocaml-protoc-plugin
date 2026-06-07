open Ocaml_protoc_plugin

module S = Reflection

let%test_module "file_descriptor" = (module
  struct
    open Google_types_pp.Descriptor.Google.Protobuf

    let fd = S.Service_info.file_descriptor

    let%test "file_name" =
      fd.name = Some "reflection.proto"

    let%test "has_services" =
      List.length fd.service = 2

    let%test "file_descriptor_proto_roundtrip" =
      let fd' = Reader.create S.Service_info.file_descriptor_proto |> FileDescriptorProto.from_proto_exn in
      fd.name = fd'.name
  end)
