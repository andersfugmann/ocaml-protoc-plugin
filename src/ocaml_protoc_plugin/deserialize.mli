val deserialize:
  ('constr, 'a) Spec.compound_list lazy_t ->
  'constr -> Reader.t -> 'a

(** **)
val deserialize_full:
  ('constr, 'a) Spec.compound_list lazy_t ->
  'constr -> Reader.t -> 'a

val deserialize_fast:
  ('constr, 'a) Spec.compound_list lazy_t ->
  'constr -> Reader.t -> 'a
(** **)
