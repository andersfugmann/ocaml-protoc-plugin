type t = { enum_names:bool; json_names:bool; omit_default_values:bool }

(**
   Create options for json serialization.

   If [enum_names] is true then enums are serialized as strings. If false the integer value is used when serializing.

   If [json_name] is true then serialization will use the json field names. If false, the fields names will be used from the protofile as-is.

   If [omit_default_values] is false then default scalar values will not be emitted to the json. The default is to omit default values.
*)
let make ?(enum_names=true) ?(json_names=true) ?(omit_default_values=true) () =
  { enum_names; json_names; omit_default_values }
let default = make ()

(**/**)

(** Perfect hash function *)
let to_int { enum_names; json_names; omit_default_values } =
  let b n = function true -> n | false -> 0 in
  b 4 enum_names + b 2 json_names + b 1 omit_default_values

let of_int n =
  let b v n = n land v = v in
  { enum_names = b 4 n; json_names = b 2 n; omit_default_values = b 1 n;   }

let max_int = to_int { enum_names=true; json_names=true; omit_default_values=true }

let%test "perfect hash" = let i = 0 in to_int (of_int i) = i
let%test "perfect hash" = let i = 1 in to_int (of_int i) = i
let%test "perfect hash" = let i = 2 in to_int (of_int i) = i
let%test "perfect hash" = let i = 3 in to_int (of_int i) = i
let%test "perfect hash" = let i = 4 in to_int (of_int i) = i
let%test "perfect hash" = let i = 5 in to_int (of_int i) = i
let%test "perfect hash" = let i = 6 in to_int (of_int i) = i
let%test "perfect hash" = let i = 7 in to_int (of_int i) = i

(**/**)
