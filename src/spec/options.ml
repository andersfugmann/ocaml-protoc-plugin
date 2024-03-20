(********************************************************)
(*           AUTOGENERATED FILE - DO NOT EDIT!          *)
(********************************************************)
(* Generated by: ocaml-protoc-plugin                    *)
(* https://github.com/andersfugmann/ocaml-protoc-plugin *)
(********************************************************)
(*
  Source: options.proto
  Syntax: proto3
  Parameters:
    debug=false
    annot=''
    opens=[]
    int64_as_int=true
    int32_as_int=true
    fixed_as_int=false
    singleton_record=false
    prefix_output_with_package=false
*)
[@@@ocaml.alert "-protobuf"] (* Disable deprecation warnings for protobuf*)

(**/**)
module Runtime' = Ocaml_protoc_plugin [@@warning "-33"]
module Imported'modules = struct
  module Descriptor = Descriptor
end
(**/**)
module rec Options : sig
  val name: unit -> string
  type t = (bool)
  val make: ?mangle_names:bool -> unit -> t
  val merge: t -> t -> t
  val to_proto': Runtime'.Writer.t -> t -> unit
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  val from_proto_exn: Runtime'.Reader.t -> t
  val to_json: Runtime'.Json_options.t -> t -> Runtime'.Json.t
  val from_json_exn: Runtime'.Json.t -> t
  val from_json: Runtime'.Json.t -> (t, [> Runtime'.Result.error]) result
end = struct
  let name () = ".Options"
  type t = (bool)
  let make ?(mangle_names = false) () = (mangle_names)
  let merge =
    let merge_mangle_names = Runtime'.Merge.merge Runtime'.Spec.( basic ((1, "mangle_names", "mangleNames"), bool, (false)) ) in
    fun (t1_mangle_names) (t2_mangle_names) -> merge_mangle_names t1_mangle_names t2_mangle_names
  let spec () = Runtime'.Spec.( basic ((1, "mangle_names", "mangleNames"), bool, (false)) ^:: nil )
  let to_proto' =
    let serialize = Runtime'.Serialize.serialize (spec ()) in
    fun writer (mangle_names) -> serialize writer mangle_names

  let to_proto t = let writer = Runtime'.Writer.init () in to_proto' writer t; writer
  let from_proto_exn =
    let constructor mangle_names = (mangle_names) in
    Runtime'.Deserialize.deserialize (spec ()) constructor
  let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)
  let to_json options =
    let serialize = Runtime'.Serialize_json.serialize ~message_name:(name ()) (spec ()) options in
    fun (mangle_names) -> serialize mangle_names
  let from_json_exn =
    let constructor mangle_names = (mangle_names) in
    Runtime'.Deserialize_json.deserialize ~message_name:(name ()) (spec ()) constructor
  let from_json json = Runtime'.Result.catch (fun () -> from_json_exn json)
end
and Ocaml_options : sig
  type t = Options.t option
  val get_exn: Imported'modules.Descriptor.Google.Protobuf.FileOptions.t -> Options.t option
  val get: Imported'modules.Descriptor.Google.Protobuf.FileOptions.t -> (Options.t option, [> Runtime'.Result.error]) result
  val set: Imported'modules.Descriptor.Google.Protobuf.FileOptions.t -> Options.t option -> Imported'modules.Descriptor.Google.Protobuf.FileOptions.t
end = struct
  type t = Options.t option
  let get_exn extendee = Runtime'.Extensions.get Runtime'.Spec.(basic_opt ((1074, "ocaml_options", "ocamlOptions"), (message (module Options)))) (extendee.Imported'modules.Descriptor.Google.Protobuf.FileOptions.extensions')
  let get extendee = Runtime'.Result.catch (fun () -> get_exn extendee)
  let set extendee t =
    let extensions' = Runtime'.Extensions.set Runtime'.Spec.(basic_opt ((1074, "ocaml_options", "ocamlOptions"), (message (module Options)))) (extendee.Imported'modules.Descriptor.Google.Protobuf.FileOptions.extensions') t in
    { extendee with Imported'modules.Descriptor.Google.Protobuf.FileOptions.extensions' = extensions' } [@@warning "-23"]

end
