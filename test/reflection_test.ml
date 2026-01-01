open Ocaml_protoc_plugin

module S = Reflection
module P = Reflection_parts

let%test _ =
  S.Service_info.file_name = "reflection.proto"

let%test _ =
  P.Service_info.file_name ="reflection_parts.proto" 

let%test "package service path" =
  S.Service_info.package_service_names = [ "test.reflection.EmptyService"; "test.reflection.SomeService" ]


let%test_module "Construct service_info by itself" = (module
  struct
    open Google_types_pp.Descriptor.Google.Protobuf
    let spec = Reader.create S.Service_info.file_descriptor_proto |> FileDescriptorProto.from_proto_exn
    let fd = FileDescriptorProto.to_proto spec |> Writer.contents |> String.escaped

    let package = Option.value spec.package ~default:""

    let%test "file_name" =
      spec.name = Some S.Service_info.file_name

    let%test "file_descriptor_proto" =
      fd = String.escaped S.Service_info.file_descriptor_proto

    let%test "package_service_names" =
      let services =
        spec.service
        |> List.map
        @@ fun ServiceDescriptorProto.{name; _} -> Option.fold name ~none:"" ~some:(Printf.sprintf "%s.%s" package)
      in
      services = S.Service_info.package_service_names
  end)
