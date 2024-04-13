open StdLabels
open MoreLabels

let failwith_f fmt =
  Printf.ksprintf (fun s -> failwith s) fmt

module String = struct
  include String

  let trim_end ~chars s =
    let chars = String.to_seq chars |> List.of_seq in
    let len = String.length s in
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
    | n -> String.sub ~pos:0 ~len:(String.length s - n) s

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
      let acc = match List.is_empty group with
        | true -> acc
        | false -> (last, List.rev group) :: acc
      in
      acc
    in
    let rec inner acc group last = function
      | x :: xs when f x = last || x = "" ->
        inner acc (x :: group) last xs
      | x :: xs ->
        inner (prepend acc group last) [x] (not last) xs
      | [] -> List.rev (prepend acc group last)
    in
    inner [] [] false lines

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
