open Spec.Descriptor.Google.Protobuf
val parse_proto_file:
  params:Parameters.t ->
  scope:Scope.t -> type_db:Type_db.t ->
  refl_service_db:(string, int) MoreLabels.Hashtbl.t ->
  (FileDescriptorProto.t * int) -> string * Code.t

val reflection :
  file_descriptors:string Dynarray.t ->
  refl_file_db:(string, int) MoreLabels.Hashtbl.t ->
  refl_service_db:(string, int) MoreLabels.Hashtbl.t -> Code.t
