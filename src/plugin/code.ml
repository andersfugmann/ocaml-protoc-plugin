open !StdLabels
open !MoreLabels
open !Utils

type indent = [ `Begin | `End | `EndBegin | `None | `Raw ]

type t = {
  mutable indent : int;
  mutable code : (indent * string) list;
}

let init () = {indent = 0; code = []}
let incr t = t.indent <- t.indent + 1
let decr t =
  match t.indent = 0 with
  | true -> failwith "Cannot decr indentation level at this point"
  | false -> t.indent <- t.indent - 1

let emit t indent fmt =
  (* Verify indentation level *)
  (match indent with
   | `Begin -> incr t
   | `End -> decr t
   | `EndBegin -> decr t; incr t
   | `None -> ()
   | `Raw -> ()
  );

  let emit s =
    String.split_on_char ~sep:'\n' s
    |> List.iter ~f:(fun s -> t.code <- (indent, String.trim_end ~chars:" \t" s) :: t.code)
  in
  Printf.ksprintf emit fmt

let contents t =
  let append buffer indent s =
    (match String.length s > 0 with
    | true ->
      List.iter ~f:(Buffer.add_string buffer) indent;
      Buffer.add_string buffer s
    | false -> ()
    );
    Buffer.add_string buffer "\n";
    buffer
  in

  let rec print buffer indent = function
    | (`None, s) :: lines -> print (append buffer indent s) indent lines
    | (`Begin, s) :: lines -> print (append buffer indent s) ("  " :: indent) lines
    | (`EndBegin, s) :: lines ->
      let indent' = List.tl indent in
      print (append buffer indent' s) indent lines
    | (`End, s) :: lines ->
      let indent = List.tl indent in
      print (append buffer indent s) indent lines
    | (`Raw, s) :: lines ->
      print (append buffer [] s) indent lines
    | [] ->
      Buffer.contents buffer
  in
  print (Buffer.create 256) [] (List.rev t.code)

let append t code =
  (* Same as rev_append no??? *)
  List.iter ~f:(fun l -> t.code <- l :: t.code) (code.code |> List.rev)

let append_deprecaton_if ~deprecated level str =
  match deprecated with
  | false -> str
  | true ->
    let level = match level with
      | `Attribute -> "@"
      | `Item -> "@@"
      | `Floating -> "@@@"
    in
    Printf.sprintf "%s[%socaml.alert protobuf \"Marked as deprecated in the .proto file\"]" str level

let emit_deprecation ?(deprecated=true) t level =
  if deprecated then
    emit t `None "%s" (append_deprecaton_if ~deprecated:true level "")

let emit_comment ~(position:[`Leading | `Trailing]) t = function
  | None -> ()
  | Some comments ->
    if position = `Leading then emit t `None "";
    let comment_string = Comment_db.to_ocaml_doc comments in
    emit t `Begin "(**";
    emit t `Raw "%s" comment_string;
    emit t `End "*)";
    if position = `Trailing then emit t `None "";
    ()

(** Emit comment for muliple fields / constructors *)
let emit_field_doc t
      ~(position:[`Leading | `Trailing])
      ?(format:('a -> 'b, unit, string, unit) format4="[%s]")
      ?(header="")
      ?(comments)
      param_comments =

  (* Remove parameters with no comments *)
  let has_header = String.length header > 0 in

  match comments, List.is_empty param_comments with
  | None, true -> ()
  | _  ->
    if position = `Leading then emit t `None "";
    emit t `Begin "(**";
    Option.iter ~f:(fun comments -> emit t `Raw "%s" (Comment_db.to_ocaml_doc comments)) comments;
    if has_header then emit t `None "%s" header;
    List.iter ~f:(fun (param, comments) ->
      emit t `None "";
      emit t `Begin format param;
      emit t `Raw "%s" (Comment_db.to_ocaml_doc comments);
      emit t `End "";
    ) param_comments;

    emit t `End "*)";
    if position = `Trailing then emit t `None ""
