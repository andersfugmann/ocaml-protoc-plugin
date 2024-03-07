(** Merge a two values. Need to match on the spec to merge messages recursivly *)
let merge: type t v. (t, v) Spec.compound -> t -> t -> t = fun spec t t' -> match spec with
  | Spec.Basic (_field, _spec, default) when t' = default -> t
  | Spec.Basic (_field, _spec, _) -> t'

    (* The spec states that proto2 required fields must be transmitted exactly once.
       So merging these fields is not possible. The essentially means that you cannot merge
       proto2 messages containing required fields.
       In this implementation, we choose to ignore this, and adopt 'keep last'
    *)
  | Spec.Basic_req (_field, Message (module Message)) -> Message.merge t t'
  | Spec.Basic_req (_field, _spec) -> t'
  | Spec.Basic_opt (_field, Message (module Message)) ->
    begin
      match t, t' with
      | None, None -> None
      | Some t, None -> Some t
      | None, Some t -> Some t
      | Some t, Some t' -> Some (Message.merge t t')
    end
  | Spec.Basic_opt (_field, _spec) -> begin
      match t' with
      | Some _ -> t'
      | None -> t
    end
  | Spec.Repeated (_field, _, _) -> t @ t'
  | Spec.Map (_field, _) -> t @ t'
  (* | Spec.Oneof _ when t' = `not_set -> t *)
  | Spec.Oneof _ -> failwith "Implementation is part of generated code"
