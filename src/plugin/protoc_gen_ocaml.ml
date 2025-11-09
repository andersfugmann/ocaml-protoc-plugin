open StdLabels
module Descriptor = Spec.Descriptor.Google.Protobuf
module Plugin = Spec.Plugin.Google.Protobuf.Compiler

(* [input] may return fewer bytes than requested before the end of input;
   only 0 means end of file. A pipe can hand over a short first chunk
   (512 bytes on macOS), so stopping at the first short read would cut
   the request that protoc sends. *)
let read_all in_channel =
  let buffer = Buffer.create 1024 in
  let chunk = Bytes.create 1024 in
  let rec inner () =
    match input in_channel chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents buffer
    | read ->
      Buffer.add_subbytes buffer chunk 0 read;
      inner ()
  in
  inner ()

(* Read from stdin *)
let read () =
  set_binary_mode_in stdin true;
  read_all stdin
  |> Ocaml_protoc_plugin.Reader.create
  |> Plugin.CodeGeneratorRequest.from_proto_exn

(* Write to stdout *)
let write response =
  set_binary_mode_out stdout true;
  Plugin.CodeGeneratorResponse.to_proto response
  |> Ocaml_protoc_plugin.Writer.contents
  |> output_string stdout

let parse_request params  files_to_generate proto_files =
  let target_proto_files = List.filter ~f:(fun Descriptor.FileDescriptorProto.{name; _} ->
    List.mem ~set:files_to_generate (Option.value_exn name)
  ) proto_files
  in
  let type_db = Type_db.init ~params proto_files in
  let fds = Dynarray.create () in
  let refl_service_db = Hashtbl .create 16 in
  let refl_file_db = Hashtbl.create 16 in

  let result =
    List.mapi ~f:(fun idx (proto_file : Descriptor.FileDescriptorProto.t) ->
      let file_descriptor_bytes = let v = Spec.Descriptor.Google.Protobuf.FileDescriptorProto.to_proto proto_file in
        Ocaml_protoc_plugin.Writer.contents v |> String.escaped
      in
      let proto_file_name = Option.value_exn ~message:"All files must have a name" proto_file.name in
      Hashtbl.add refl_file_db proto_file_name idx;
      Dynarray.add_last fds file_descriptor_bytes;
      let scope = Scope.init ~module_name:(Type_db.get_module_name type_db proto_file_name) in
      Emit.parse_proto_file ~params ~scope ~type_db ~refl_service_db (proto_file, idx)
    ) target_proto_files
  in
  let result_reflection = Emit.reflection ~file_descriptors:fds ~refl_file_db ~refl_service_db in
  (match params.debug with
   | true -> List.iter ~f:(fun (_, code) -> Printf.eprintf "%s\n%!" (Code.contents code)) result
   | false -> ());
  result, result_reflection

let () =
  let request = read () in
  try
    let Plugin.CodeGeneratorRequest.{parameter = parameters;  file_to_generate = files_to_generate; proto_file = proto_files;  _} = request in
    let params =
      Parameters.parse (Option.value ~default:"" parameters)
    in
    let (outputs, reflection) = parse_request params files_to_generate proto_files in
    let response_of_output (name, code) =
      Plugin.CodeGeneratorResponse.File.make ~name ~content:(Code.contents code) ()
    in
    let response : Plugin.CodeGeneratorResponse.t =
      Plugin.CodeGeneratorResponse.make ~supported_features:1 ~file:(List.map ~f:response_of_output outputs) ()
    in
    write response;
    if params.reflection_map  then
      let response_of_reflection = Plugin.CodeGeneratorResponse.make ~file:[Plugin.CodeGeneratorResponse.File.
        make
          ~name:"reflection_map.ml"
          ~content:(Code.contents reflection)
          ();
      ] () in
      write response_of_reflection

  with
  | Failure message -> (Printf.eprintf "%s\n" message; exit 1)
