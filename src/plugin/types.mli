open Spec.Descriptor.Google.Protobuf

type t = {
  type' : string;
  constructor: string;
  apply: (string * string) option;
  spec_str: string;
  default_constructor_sig: string;
  default_constructor_impl: string;
  merge_impl: string;
}

type field_spec = {
  typestr : string;
  spec_str: string;
}

val spec_of_field:
  params:Parameters.t ->
  syntax:[ `Proto2 | `Proto3 ] ->
  scope:Scope.t ->
  map_type:DescriptorProto.t option ->
  FieldDescriptorProto.t -> field_spec

val make:
  params:Parameters.t ->
  syntax:[ `Proto2 | `Proto3 ] ->
  is_cyclic: bool ->
  extension_ranges: (int*int) list ->
  scope:Scope.t ->
  fields:(FieldDescriptorProto.t * DescriptorProto.t option) list -> OneofDescriptorProto.t list -> t
