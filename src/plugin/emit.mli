open Spec.Descriptor.Google.Protobuf
val parse_proto_file:
  params:Parameters.t ->
  scope:Scope.t -> type_db:Type_db.t -> FileDescriptorProto.t -> string * Code.t
