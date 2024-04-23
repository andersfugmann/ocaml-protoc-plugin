open StdLabels
open MoreLabels

let failwith_f fmt =
  Printf.ksprintf (fun s -> failwith s) fmt

module String = struct
  include String

  let starts_with ~prefix s =
    let regex = Str.regexp ("^" ^ Str.quote prefix) in
    Str.string_match regex s 0

  let trim_end ~chars s =
    let chars = to_seq chars |> List.of_seq in
    let len = length s in
    let rcount s =
      let rec inner = function
        | 0 -> len
        | n when List.mem s.[n - 1] ~set:chars -> inner (n - 1)
        | n -> len - n
      in
      inner len
    in
    match rcount s with
    | 0 -> s
    | n -> sub ~pos:0 ~len:(length s - n) s

  let starts_with_regex ~regex str =
    let regex = Str.regexp ("^" ^ regex) in
    Str.string_match regex str 0

  let replace ~substring ~f =
    let regexp = Str.regexp (Str.quote substring) in
    Str.global_substitute regexp f

end

module List = struct
  include List
  let rec drop_while ~f = function
    | x :: xs when f x -> drop_while ~f xs
    | xs -> xs

  let is_empty = function
    | [] -> true
    | _ -> false

  let group ~f lines =
    let prepend acc group last =
      let acc = match is_empty group with
        | true -> acc
        | false -> (last, rev group) :: acc
      in
      acc
    in
    let rec inner acc group last = function
      | x :: xs when f x = last || x = "" ->
        inner acc (x :: group) last xs
      | x :: xs ->
        inner (prepend acc group last) [x] (not last) xs
      | [] -> rev (prepend acc group last)
    in
    inner [] [] false lines

  let filteri ~f lst =
    let rec inner i = function
      | [] -> []
      | x :: xs when f i x -> x :: inner (i+1) xs
      | _ :: xs -> inner (i+1) xs
    in
    inner 0 lst

  let rec find_map ~f = function
    | [] -> None
    | x :: xs -> match f x with Some _ as v -> v | None -> find_map ~f xs
end

module StringMap = struct
  include Map.Make(String)

(** Fail with an error if the key already exists *)
  let add_uniq ~key ~data map =
    update ~key ~f:(function
      | None -> Some data
      | Some _ -> failwith_f "Key %s already exists" key
    ) map
end
module StringSet = Set.Make(String)
