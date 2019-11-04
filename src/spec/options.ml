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

open Ocaml_protoc_plugin.Runtime
module rec Options : sig
  val name': unit -> string
  type t = bool 
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> t Runtime'.Result.t
end = struct 
  let name' () = "options.Options"
  type t = bool
  let to_proto =
    let apply = fun ~f a -> f [] a in
    let spec = Runtime'.Serialize.C.( basic (1, bool, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions a -> a in
    let spec = Runtime'.Deserialize.C.( basic (1, bool, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer
  
end
and Ocaml_options : sig
  type t = Options.t option 
  val get: Descriptor.Google.Protobuf.FileOptions.t -> Options.t option Runtime'.Result.t
  val set: Descriptor.Google.Protobuf.FileOptions.t -> Options.t option -> Descriptor.Google.Protobuf.FileOptions.t
end = struct 
  type t = Options.t option 
  let get extendee = Runtime'.Extensions.get Runtime'.Deserialize.C.( basic_opt (1074, (message Options.from_proto)) ^:: nil ) (extendee.Descriptor.Google.Protobuf.FileOptions.extensions')
  let set extendee t =
    let extensions' = Runtime'.Extensions.set (Runtime'.Serialize.C.( basic_opt (1074, (message Options.to_proto)) ^:: nil )) (extendee.Descriptor.Google.Protobuf.FileOptions.extensions') t in
    { extendee with Descriptor.Google.Protobuf.FileOptions.extensions' = extensions' }
  
end