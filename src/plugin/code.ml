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

let trim_end ~char s =
  let len = String.length s in
  let rcount s =
    let rec inner = function
      | 0 -> len
      | n when s.[n - 1] = char -> inner (n - 1)
      | n -> len - n
    in
    inner len
  in
  match rcount s with
  | 0 -> s
  | n -> String.sub ~pos:0 ~len:(String.length s - n) s

let emit t indent fmt =
  let prepend s =
    String.split_on_char ~sep:'\n' s
    |> List.iter ~f:(fun line ->
      (* Replace tabs with indent *)
      let line =
        "" :: String.split_on_char ~sep:'\t' line
        |> String.concat ~sep:t.indent
      in
      t.code <- (trim_end ~char:' ' line) :: t.code);
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

let map_comments comments =
  comments
  |> List.map ~f:(trim_end ~char:'\n')
  |> String.concat ~sep:"\n\n"
  |> String.to_seq
  |> Seq.map (function
    | '{' | '}' | '[' | ']' | '@' | '\\' as ch -> Printf.sprintf "\\%c" ch
    | ch -> Printf.sprintf "%c" ch
  )
  |> List.of_seq
  |> String.concat ~sep:""
  |> String.split_on_char ~sep:'\n'


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
    Printf.sprintf "%s[%socaml.alert protobuf \"Deprecated global\"]" str level

let deprecated_comment = "@deprecated deprecated in proto file"

let append_comments ?(deprecated=false) ~comments str =
  let comment_str =
    map_comments comments
    |> String.concat ~sep:"\n"
    |> String.trim
  in
  match comments, deprecated with
  | [], false -> str
  | [], true -> Printf.sprintf "%s(** %s *)" str deprecated_comment
  | _, false ->
    Printf.sprintf "%s(** %s *)" str comment_str
  | _, true ->
    Printf.sprintf "%s(** %s\n%s *)" str comment_str deprecated_comment

let emit_comment ?(deprecated=false) ~(position:[`Leading | `Trailing]) t = function
  | [] when not deprecated -> ()
  | comments ->
    if position = `Leading then emit t `None "";
    let comments = map_comments comments in
    let () =
      match comments with
      | [ comment ] when not deprecated -> emit t `None "(** %s *)" (String.trim comment)
      | comments ->
        emit t `Begin "(**";
        List.iter ~f:(emit t `None "%s") comments;
        if deprecated then emit t `None "%s" deprecated_comment;
        emit t `End "*)";
    in
    (* if position = `Trailing then emit t `None ""; (* Dont think this is needed *) *)
    ()

let contents t =
  List.map ~f:(Printf.sprintf "%s") (List.rev t.code)
  |> String.concat ~sep:"\n"
