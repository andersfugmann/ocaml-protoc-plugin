type comment
type t
val init: Spec.Descriptor.Google.Protobuf.FileDescriptorProto.t -> t
val get_message_comments : proto_path:string -> ?name:string -> t -> comment option
val get_field_comments : proto_path:string -> ?name:string -> t -> comment option
val get_enum_comments : proto_path:string -> ?name:string -> t -> comment option
val get_enum_value_comments : proto_path:string -> ?name:string -> t -> comment option
val get_oneof_comments : proto_path:string -> ?name:string -> t -> comment option
val get_service_comments : proto_path:string -> ?name:string -> t -> comment option
val get_method_comments : proto_path:string -> ?name:string -> t -> comment option
val get_extension_comments : proto_path:string -> ?name:string -> t -> comment option
val get_file_comments : t -> comment option
val get_option_comments : proto_path:string -> ?name:string -> t -> comment option

val to_ocaml_doc : comment -> string
