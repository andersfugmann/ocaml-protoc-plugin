(* Mimic base Option type *)
type 'a t = 'a option

let value ~default = function
  | None -> default
  | Some v -> v

let value_exn ?(message="Option is None") = function
  | None -> failwith message
  | Some v -> v

let value_map ~default ~f = function
  | None -> default
  | Some v -> f v

let map ~f = function
  | Some v -> Some (f v)
  | None -> None

let iter ~f = function
  | Some v -> f v
  | None -> ()

let bind ~f = function
  | None -> None
  | Some v -> f v

let some v = Some v

let is_some = function
  | Some _ -> true
  | None -> false

let is_none v = not (is_some v)

let none = None
