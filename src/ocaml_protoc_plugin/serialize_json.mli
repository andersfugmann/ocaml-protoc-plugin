val serialize: message_name:string -> ?enum_names:bool -> ?json_names:bool -> ?omit_default_values:bool -> ('a, Yojson.Basic.t) Spec.compound_list -> 'a
