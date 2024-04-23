type t

(** Create a new scope. I think this is never used, and should take a module name *)
val init : module_name:string -> t

(** Push an identifier to the current scope *)
val push : t -> string -> t

(** The import module name - Must be globally unique *)
val import_module_name: string

(** Name of current scope module alias *)
val this_module_alias: string

(** Get the ocaml name of the given proto type name, based on the current scope *)
val get_scoped_name_type_db: ?postfix:string -> t -> Type_db.t -> string option -> string

(** Get stringified version of the current proto path *)
val get_proto_path: ?name:string -> t -> string
