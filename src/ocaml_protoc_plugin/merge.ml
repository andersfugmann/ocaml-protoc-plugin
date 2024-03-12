(** Merge a two values. Need to match on the spec to merge messages recursivly *)
let merge: type t v. (t, v) Spec.compound -> t -> t -> t = function
  | Spec.Basic (_field, _spec, default) ->
    (fun t t' -> match t' = default with true -> t | false -> t')
    (* The spec states that proto2 required fields must be transmitted exactly once.
       So merging these fields is not possible. The essentially means that you cannot merge
       proto2 messages containing required fields.
       In this implementation, we choose to ignore this, and adopt 'keep last'
    *)
  | Spec.Basic_req (_field, Message (module Message)) ->
    Message.merge
  | Spec.Basic_req (_field, _spec) -> fun _ t' -> t'
  | Spec.Basic_opt (_field, Message (module Message)) ->
    begin
      fun t t' ->
        match t, t' with
        | None, None -> None
        | Some t, None -> Some t
        | None, Some t -> Some t
        | Some t, Some t' -> Some (Message.merge t t')
    end
  | Spec.Basic_opt (_field, _spec) -> begin
      fun t -> function
        | Some _ as t' -> t'
        | None -> t
    end
  | Spec.Repeated (_field, _, _) -> List.append
  | Spec.Map (_field, _) -> List.append
  (* | Spec.Oneof _ when t' = `not_set -> t *)
  | Spec.Oneof _ -> failwith "Implementation is part of generated code"
