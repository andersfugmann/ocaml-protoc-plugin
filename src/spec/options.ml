(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
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
*)

open Ocaml_protoc_plugin.Runtime [@@warning "-33"]
(**/**)
module Imported'modules = struct
  module Descriptor = Descriptor
end
(**/**)
module rec Options : sig
  val name': unit -> string
  type t = bool
  val make: ?mangle_names:bool -> unit -> t
  val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  val from_proto_exn: Runtime'.Reader.t -> t
end = struct
  let name' () = "options.Options"
  type t = bool
  let make ?(mangle_names = false) () = mangle_names
  let to_proto' =
    let spec = Runtime'.Serialize.C.( basic (1, bool, Some (false)) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize spec in
    serialize

  let to_proto t = to_proto' (Runtime'.Writer.init ()) t
  let from_proto_exn =
    let constructor = fun mangle_names -> mangle_names in
    let spec = Runtime'.Deserialize.C.( basic (1, bool, Some (false)) ^:: nil ) in
    Runtime'.Deserialize.deserialize spec constructor
  let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)
end
and Ocaml_options : sig
  type t = Options.t option
  val get_exn: Imported'modules.Descriptor.Google.Protobuf.FileOptions.t -> Options.t option
  val get: Imported'modules.Descriptor.Google.Protobuf.FileOptions.t -> (Options.t option, [> Runtime'.Result.error]) result
  val set: Imported'modules.Descriptor.Google.Protobuf.FileOptions.t -> Options.t option -> Imported'modules.Descriptor.Google.Protobuf.FileOptions.t
end = struct
  type t = Options.t option
  let get_exn extendee = Runtime'.Extensions.get Runtime'.Deserialize.C.(basic_opt (1074, (message (fun t -> Options.from_proto_exn t)))) (extendee.Imported'modules.Descriptor.Google.Protobuf.FileOptions.extensions')
  let get extendee = Runtime'.Result.catch (fun () -> get_exn extendee)
  let set extendee t =
    let extensions' = Runtime'.Extensions.set Runtime'.Serialize.C.(basic_opt (1074, (message (fun t -> Options.to_proto' t)))) (extendee.Imported'modules.Descriptor.Google.Protobuf.FileOptions.extensions') t in
    { extendee with Imported'modules.Descriptor.Google.Protobuf.FileOptions.extensions' = extensions' } [@@warning "-23"]

end
