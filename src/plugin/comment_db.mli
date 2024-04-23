type comment = string option
type comments = {
  leading : comment;
  trailing : comment;
  detatched : string list;
}

type t
val init: Spec.Descriptor.Google.Protobuf.FileDescriptorProto.t -> t
val get_message_comments : proto_path:string -> ?name:string -> t -> string list
val get_field_comments : proto_path:string -> ?name:string -> t -> string list
val get_enum_comments : proto_path:string -> ?name:string -> t -> string list
val get_enum_value_comments : proto_path:string -> ?name:string -> t -> string list
val get_oneof_comments : proto_path:string -> ?name:string -> t -> string list
val get_service_comments : proto_path:string -> ?name:string -> t -> string list
val get_method_comments : proto_path:string -> ?name:string -> t -> string list
val get_extension_comments : proto_path:string -> ?name:string -> t -> string list
val get_file_comments : t -> string list (* Would it have any???? *)
val get_option_comments : proto_path:string -> ?name:string -> t -> string list
