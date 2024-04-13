open StdLabels
open !MoreLabels

type t = {
  mutable indent : string;
  mutable code : string list;
}

let init () = {indent = ""; code = []}
let incr t = t.indent <- "  " ^ t.indent
let decr t =
  match String.length t.indent >= 2 with
  | true ->
    t.indent <- String.sub ~pos:0 ~len:(String.length t.indent - 2) t.indent
  | false -> failwith "Cannot decr indentation level at this point"

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

let rec drop_while ~f = function
  | x :: xs when f x -> drop_while ~f xs
  | xs -> xs

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

let starts_with_regex ~regex str =
  let regex = Str.regexp ("^" ^ regex) in
  Str.string_match regex str 0

(** Merge groups when the list groups ends with a line that starts with a '-' *)
let rec merge_list_groups = function
  | (false, l1) :: (true, l2) :: xs ->
    begin match List.rev l1 with
    | s :: _ when starts_with_regex ~regex:"[ ]*- " s ->
      (false, l1 @ l2) :: merge_list_groups xs
    | _ -> (false, l1) :: (true, l2) :: merge_list_groups xs
    end
  | x :: xs ->
    x :: merge_list_groups xs
  | [] -> []

let replace ~substring ~f =
  let regexp = Str.regexp (Str.quote substring) in
  Str.global_substitute regexp f

let remove_trailing_empty_lines lines =
  lines
  |> List.rev
  |> drop_while ~f:((=) "")
  |> List.rev

let escape_comment s =
  String.to_seq s
  |> Seq.map (function
    | '{' | '}' | '[' | ']' | '@' | '\\' as ch -> Printf.sprintf "\\%c" ch
    | ch -> Printf.sprintf "%c" ch
  )
  |> List.of_seq
  |> String.concat ~sep:""


let map_comments comments =
  comments
  |> String.concat ~sep:"\n\n"
  |> String.split_on_char ~sep:'\n'
  |> List.map ~f:(trim_end ~chars:" \n\t")
  |> group ~f:(fun s -> String.starts_with ~prefix:"  " s && not (starts_with_regex ~regex:"[ ]*- " s))
  |> merge_list_groups
  |> List.map ~f:(function
    | (false, lines) ->
      lines
      |> List.map ~f:String.trim
      |> remove_trailing_empty_lines
      |> List.map ~f:escape_comment
    | (true, lines) ->
      let lines =
        lines
        |> List.map ~f:(replace ~substring:"v}" ~f:(fun _ -> "v\\}"))
        |> remove_trailing_empty_lines
      in
      (* TODO: Remove indentation *)
      "{v" :: lines @ ["v}"]
  )
  |> List.flatten
  |> List.rev
  |> drop_while ~f:(fun x -> x = "")
  |> List.rev

let emit t indent fmt =
  let prepend s =
    String.split_on_char ~sep:'\n' s
    |> List.iter ~f:(fun line ->
      (* Replace tabs with indent *)
      let line =
        "" :: String.split_on_char ~sep:'\t' line
        |> String.concat ~sep:t.indent
      in
      t.code <- (trim_end ~chars:" " line) :: t.code);
  in
  let emit s =
    match indent with
    | `Begin ->
      prepend s;
      incr t
    | `None ->
      prepend s
    | `End ->
      decr t;
      prepend s
    | `EndBegin ->
      decr t;
      prepend s;
      incr t
  in
  Printf.ksprintf emit fmt

let append t code = List.iter ~f:(emit t `None "%s") (code.code |> List.rev)

let append_deprecaton_if ~deprecated level str =
  match deprecated with
  | false -> str
  | true ->
    let level = match level with
      | `Attribute-> "@"
      | `Item -> "@@"
      | `Floating -> "@@@"
    in
    Printf.sprintf "%s[%socaml.alert protobuf \"Marked as deprecated in the .proto file\"]" str level

let append_comments ~comments str =
  let comment_str =
    map_comments comments
    |> String.concat ~sep:"\n"
    |> String.trim
  in
  match List.is_empty comments with
  | true -> str
  | false ->
    Printf.sprintf "%s(** %s *)" str comment_str

let emit_comment ~(position:[`Leading | `Trailing]) t = function
  | [] -> ()
  | comments ->
    if position = `Leading then emit t `None "";
    let comments = map_comments comments in
    let () =
      match comments with
      | [ comment ] -> emit t `None "(** %s *)" (String.trim comment)
      | comments ->
        emit t `Begin "(**";
        List.iter ~f:(emit t `None "%s") comments;
        emit t `End "*)";
    in
    (* if position = `Trailing then emit t `None ""; (* Dont think this is needed *) *)
    ()

let contents t =
  List.map ~f:(Printf.sprintf "%s") (List.rev t.code)
  |> String.concat ~sep:"\n"
