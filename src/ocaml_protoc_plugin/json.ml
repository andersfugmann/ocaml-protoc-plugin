open StdLabels

(** Json type. This is identical to Yojson.Basic.t *)
type t = [
  | `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
]

let rec to_string: t -> string = function
  | `Null -> "null"
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> Printf.sprintf "\"%s\"" s
  | `Assoc l -> List.map ~f:(fun (key, value) -> Printf.sprintf "\"%s\": %s" key (to_string value)) l
                |> String.concat ~sep:", "
                |> Printf.sprintf "{ %s }"
  | `List l -> List.map ~f:to_string l
               |> String.concat ~sep:", "
               |> Printf.sprintf "[ %s ]"
