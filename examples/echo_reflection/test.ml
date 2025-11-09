open Echo

let () =
  assert (List.mem Echo.Echo.package_service_name Reflection_map.services);
  assert (Reflection_map.get_file_descriptor "echo.proto" |> Option.is_some);
  assert (Reflection_map.get_service_descriptor "echo.Echo" |> Option.is_some);
