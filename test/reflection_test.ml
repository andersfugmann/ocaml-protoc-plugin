module S = Reflection
module P = Reflection_parts

let%test _ =
  S.Service_info.file_name = "reflection.proto"

let%test _ =
  S.Service_info.package_service_names = [ "test.reflection.EmptyService"; "test.reflection.SomeService" ]

let%test _ =
  P.Service_info.file_name ="reflection_parts.proto"

let%test _ =
  List.is_empty P.Service_info.package_service_names

let%test_module "Construct service_info by itself" = (module
  struct
    open Google_types_pp.Descriptor.Google.Protobuf

    let package = S.Service_info.file_descriptor.package |> Option.get

    let%test "file_name" =
      S.Service_info.file_name = (S.Service_info.file_descriptor.name |> Option.get)

    let%test "package_service_names" =
      let services =
        S.Service_info.file_descriptor.service
        |> List.map
        @@ fun ServiceDescriptorProto.{name; _} -> Option.fold name ~none:"" ~some:(Printf.sprintf "%s.%s" package)
      in
      services = S.Service_info.package_service_names
  end)
