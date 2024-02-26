(********************************************************)
(*           AUTOGENERATED FILE - DO NOT EDIT!          *)
(********************************************************)
(* Generated by: ocaml-protoc-plugin                    *)
(* https://github.com/andersfugmann/ocaml-protoc-plugin *)
(********************************************************)
(*
  Source: google/protobuf/compiler/plugin.proto
  Syntax: proto2
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
module rec Google : sig
  module rec Protobuf : sig
    module rec Compiler : sig
      module rec Version : sig
        val name': unit -> string
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        val make: ?major:int -> ?minor:int -> ?patch:int -> ?suffix:string -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
      and CodeGeneratorRequest : sig
        val name': unit -> string
        type t = { file_to_generate: string list; parameter: string option; compiler_version: Version.t option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list }
        val make: ?file_to_generate:string list -> ?parameter:string -> ?compiler_version:Version.t -> ?proto_file:Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
      and CodeGeneratorResponse : sig
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
          val to_string: t -> string
          val from_string_exn: string -> t
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make: ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val merge: t -> t -> t
          val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end
        val name': unit -> string
        type t = { error: string option; supported_features: int option; file: File.t list }
        val make: ?error:string -> ?supported_features:int -> ?file:File.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
    end
  end
end = struct
  module rec Protobuf : sig
    module rec Compiler : sig
      module rec Version : sig
        val name': unit -> string
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        val make: ?major:int -> ?minor:int -> ?patch:int -> ?suffix:string -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
      and CodeGeneratorRequest : sig
        val name': unit -> string
        type t = { file_to_generate: string list; parameter: string option; compiler_version: Version.t option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list }
        val make: ?file_to_generate:string list -> ?parameter:string -> ?compiler_version:Version.t -> ?proto_file:Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
      and CodeGeneratorResponse : sig
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
          val to_string: t -> string
          val from_string_exn: string -> t
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make: ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val merge: t -> t -> t
          val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end
        val name': unit -> string
        type t = { error: string option; supported_features: int option; file: File.t list }
        val make: ?error:string -> ?supported_features:int -> ?file:File.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
    end
  end = struct
    module rec Compiler : sig
      module rec Version : sig
        val name': unit -> string
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        val make: ?major:int -> ?minor:int -> ?patch:int -> ?suffix:string -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
      and CodeGeneratorRequest : sig
        val name': unit -> string
        type t = { file_to_generate: string list; parameter: string option; compiler_version: Version.t option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list }
        val make: ?file_to_generate:string list -> ?parameter:string -> ?compiler_version:Version.t -> ?proto_file:Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
      and CodeGeneratorResponse : sig
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
          val to_string: t -> string
          val from_string_exn: string -> t
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make: ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val merge: t -> t -> t
          val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end
        val name': unit -> string
        type t = { error: string option; supported_features: int option; file: File.t list }
        val make: ?error:string -> ?supported_features:int -> ?file:File.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end
    end = struct
      module rec Version : sig
        val name': unit -> string
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        val make: ?major:int -> ?minor:int -> ?patch:int -> ?suffix:string -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end = struct
        let name' () = "plugin.google.protobuf.compiler.Version"
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        let make ?major ?minor ?patch ?suffix () = { major; minor; patch; suffix }
        let merge = (fun t1 t2 -> {
          major = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((1, "major", "major"), int32_int) ) t1.major t2.major);
          minor = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((2, "minor", "minor"), int32_int) ) t1.minor t2.minor);
          patch = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((3, "patch", "patch"), int32_int) ) t1.patch t2.patch);
          suffix = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((4, "suffix", "suffix"), string) ) t1.suffix t2.suffix);
           })
        let spec () = Runtime'.Spec.( basic_opt ((1, "major", "major"), int32_int) ^:: basic_opt ((2, "minor", "minor"), int32_int) ^:: basic_opt ((3, "patch", "patch"), int32_int) ^:: basic_opt ((4, "suffix", "suffix"), string) ^:: nil )
        let to_proto' =
          let serialize = Runtime'.Serialize.serialize (spec ()) in
          fun writer { major; minor; patch; suffix } -> serialize writer major minor patch suffix

        let to_proto t = to_proto' (Runtime'.Writer.init ()) t
        let from_proto_exn =
          let constructor = fun major minor patch suffix -> { major; minor; patch; suffix } in
          Runtime'.Deserialize.deserialize (spec ()) constructor
        let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)
      end
      and CodeGeneratorRequest : sig
        val name': unit -> string
        type t = { file_to_generate: string list; parameter: string option; compiler_version: Version.t option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list }
        val make: ?file_to_generate:string list -> ?parameter:string -> ?compiler_version:Version.t -> ?proto_file:Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end = struct
        let name' () = "plugin.google.protobuf.compiler.CodeGeneratorRequest"
        type t = { file_to_generate: string list; parameter: string option; compiler_version: Version.t option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list }
        let make ?(file_to_generate = []) ?parameter ?compiler_version ?(proto_file = []) () = { file_to_generate; parameter; compiler_version; proto_file }
        let merge = (fun t1 t2 -> {
          file_to_generate = (Runtime'.Merge.merge Runtime'.Spec.( repeated ((1, "file_to_generate", "fileToGenerate"), string, not_packed) ) t1.file_to_generate t2.file_to_generate);
          parameter = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((2, "parameter", "parameter"), string) ) t1.parameter t2.parameter);
          compiler_version = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((3, "compiler_version", "compilerVersion"), (message (module Version))) ) t1.compiler_version t2.compiler_version);
          proto_file = (Runtime'.Merge.merge Runtime'.Spec.( repeated ((15, "proto_file", "protoFile"), (message (module Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto)), not_packed) ) t1.proto_file t2.proto_file);
           })
        let spec () = Runtime'.Spec.( repeated ((1, "file_to_generate", "fileToGenerate"), string, not_packed) ^:: basic_opt ((2, "parameter", "parameter"), string) ^:: basic_opt ((3, "compiler_version", "compilerVersion"), (message (module Version))) ^:: repeated ((15, "proto_file", "protoFile"), (message (module Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto)), not_packed) ^:: nil )
        let to_proto' =
          let serialize = Runtime'.Serialize.serialize (spec ()) in
          fun writer { file_to_generate; parameter; compiler_version; proto_file } -> serialize writer file_to_generate parameter compiler_version proto_file

        let to_proto t = to_proto' (Runtime'.Writer.init ()) t
        let from_proto_exn =
          let constructor = fun file_to_generate parameter compiler_version proto_file -> { file_to_generate; parameter; compiler_version; proto_file } in
          Runtime'.Deserialize.deserialize (spec ()) constructor
        let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)
      end
      and CodeGeneratorResponse : sig
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
          val to_string: t -> string
          val from_string_exn: string -> t
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make: ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val merge: t -> t -> t
          val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end
        val name': unit -> string
        type t = { error: string option; supported_features: int option; file: File.t list }
        val make: ?error:string -> ?supported_features:int -> ?file:File.t list -> unit -> t
        val merge: t -> t -> t
        val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end = struct
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
          val to_string: t -> string
          val from_string_exn: string -> t
        end = struct
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          let to_int = function
            | FEATURE_NONE -> 0
            | FEATURE_PROTO3_OPTIONAL -> 1
          let from_int_exn = function
            | 0 -> FEATURE_NONE
            | 1 -> FEATURE_PROTO3_OPTIONAL
            | n -> Runtime'.Result.raise (`Unknown_enum_value n)
          let from_int e = Runtime'.Result.catch (fun () -> from_int_exn e)
          let to_string = function
            | FEATURE_NONE -> "FEATURE_NONE"
            | FEATURE_PROTO3_OPTIONAL -> "FEATURE_PROTO3_OPTIONAL"
          let from_string_exn = function
            | "FEATURE_NONE" -> FEATURE_NONE
            | "FEATURE_PROTO3_OPTIONAL" -> FEATURE_PROTO3_OPTIONAL
            | s -> failwith ("Unknown enum name" ^ s)

        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make: ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val merge: t -> t -> t
          val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end = struct
          let name' () = "plugin.google.protobuf.compiler.CodeGeneratorResponse.File"
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          let make ?name ?insertion_point ?content ?generated_code_info () = { name; insertion_point; content; generated_code_info }
          let merge = (fun t1 t2 -> {
            name = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((1, "name", "name"), string) ) t1.name t2.name);
            insertion_point = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((2, "insertion_point", "insertionPoint"), string) ) t1.insertion_point t2.insertion_point);
            content = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((15, "content", "content"), string) ) t1.content t2.content);
            generated_code_info = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((16, "generated_code_info", "generatedCodeInfo"), (message (module Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo))) ) t1.generated_code_info t2.generated_code_info);
             })
          let spec () = Runtime'.Spec.( basic_opt ((1, "name", "name"), string) ^:: basic_opt ((2, "insertion_point", "insertionPoint"), string) ^:: basic_opt ((15, "content", "content"), string) ^:: basic_opt ((16, "generated_code_info", "generatedCodeInfo"), (message (module Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo))) ^:: nil )
          let to_proto' =
            let serialize = Runtime'.Serialize.serialize (spec ()) in
            fun writer { name; insertion_point; content; generated_code_info } -> serialize writer name insertion_point content generated_code_info

          let to_proto t = to_proto' (Runtime'.Writer.init ()) t
          let from_proto_exn =
            let constructor = fun name insertion_point content generated_code_info -> { name; insertion_point; content; generated_code_info } in
            Runtime'.Deserialize.deserialize (spec ()) constructor
          let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)
        end
        let name' () = "plugin.google.protobuf.compiler.CodeGeneratorResponse"
        type t = { error: string option; supported_features: int option; file: File.t list }
        let make ?error ?supported_features ?(file = []) () = { error; supported_features; file }
        let merge = (fun t1 t2 -> {
          error = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((1, "error", "error"), string) ) t1.error t2.error);
          supported_features = (Runtime'.Merge.merge Runtime'.Spec.( basic_opt ((2, "supported_features", "supportedFeatures"), uint64_int) ) t1.supported_features t2.supported_features);
          file = (Runtime'.Merge.merge Runtime'.Spec.( repeated ((15, "file", "file"), (message (module File)), not_packed) ) t1.file t2.file);
           })
        let spec () = Runtime'.Spec.( basic_opt ((1, "error", "error"), string) ^:: basic_opt ((2, "supported_features", "supportedFeatures"), uint64_int) ^:: repeated ((15, "file", "file"), (message (module File)), not_packed) ^:: nil )
        let to_proto' =
          let serialize = Runtime'.Serialize.serialize (spec ()) in
          fun writer { error; supported_features; file } -> serialize writer error supported_features file

        let to_proto t = to_proto' (Runtime'.Writer.init ()) t
        let from_proto_exn =
          let constructor = fun error supported_features file -> { error; supported_features; file } in
          Runtime'.Deserialize.deserialize (spec ()) constructor
        let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)
      end
    end
  end
end
