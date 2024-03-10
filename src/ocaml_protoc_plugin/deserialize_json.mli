val deserialize: ('constr, 'a) Spec.compound_list -> 'constr -> Yojson.Basic.t -> 'a

(**)
val to_int64: Yojson.Basic.t -> int64
val to_int32: Yojson.Basic.t -> int32
val to_int: Yojson.Basic.t -> int
val to_string: Yojson.Basic.t -> string
val to_bytes: Yojson.Basic.t -> bytes
val to_float: Yojson.Basic.t -> float
val to_bool: Yojson.Basic.t -> bool
val to_list: Yojson.Basic.t -> Yojson.Basic.t list

(**)
