open StdLabels
open Ocaml_protoc_plugin


module Reference = struct
  let google_include_path =
    let ch = open_in "google_include" in
    let include_path = input_line ch in
    close_in ch;
    include_path

  external protobuf2json : google_include_path:string -> ?proto_file:string -> message_type:string -> string -> string = "protobuf2json"
  let to_json ?proto_file ~message_type data =
    protobuf2json ~google_include_path ?proto_file ~message_type data
    |> Yojson.Basic.from_string

  external json2protobuf : google_include_path:string -> ?proto_file:string -> message_type:string -> string -> string = "json2protobuf"
  let from_json ?proto_file ~message_type json =
    let data = Yojson.Basic.to_string json in
    json2protobuf ~google_include_path ?proto_file ~message_type data

end

module type T = sig
  type t [@@deriving show, eq]
  val to_proto' : Writer.t -> t -> unit
  val to_proto : t -> Writer.t
  val from_proto : Reader.t -> t Result.t
  val from_proto_exn : Reader.t -> t
  val name : unit -> string
  val merge: t -> t -> t
  val to_json: Json_options.t -> t -> Yojson.Basic.t
  val from_json_exn: Yojson.Basic.t -> t
  val from_json: Yojson.Basic.t -> t Result.t
end

let hexlify data =
  let acc = ref [] in
  String.iter ~f:(fun ch -> (acc := Char.code ch :: !acc)) data;
  List.rev !acc
  |> List.map ~f:(Printf.sprintf "%02x")
  |> String.concat ~sep:"-"
  |> Printf.printf "Buffer: '%s'\n"

let dump_protoc ?(protoc_args=[]) ~proto_file type_name data =
  let filename = Filename.temp_file type_name ".bin" in
  let cout = open_out filename in
  output_string cout data;
  close_out cout;
  Printf.printf "%!";
  let command =
    Printf.sprintf "protoc %s --decode=%s %s < %s 2>/dev/null"
      (String.concat ~sep:" " protoc_args)
      type_name
      proto_file
      filename
  in
  let res = Sys.command command in
  Sys.remove filename;
  match res with
  | 0 -> ()
  | n -> Printf.printf "'protoc' exited with status code: %d. \n%s\n" n command

let test_merge (type t) (module M : T with type t = t) (t: t) =
  Test_runtime.set_stragegy Test_runtime.Standard;
  let iterations = [1;2;3;4] in
  let writer = Writer.init () in
  let _ =
    List.fold_left ~init:(writer, t) ~f:(fun (writer, expect) i ->
      M.to_proto' writer t;
      let contents = Writer.contents writer |> Reader.create in
      let () =
        match M.from_proto contents with
        | Error err ->  Printf.printf "Error decoding after %d iterations: %s\n" i (Result.show_error err)
        | Ok observed when M.equal expect observed -> ()
        | Ok observed ->
          Printf.printf "Wrong value after %d iterations\nExpect: %s\nObserved:%s\n" i ([%show: M.t] expect) ([%show: M.t] observed)
      in
      (writer, M.merge expect t)
    ) iterations
  in
  ()

let test_json ~debug ~proto_file (type t) (module M : T with type t = t) (t: t) =
  let message_type =
    match M.name () |> String.split_on_char ~sep:'.' with
    | _ :: tl ->
      let message_type = String.concat ~sep:"." tl in
      message_type
    | _ -> failwith "Illegal name"
  in
  ignore debug;
  let json_ref t =
    let proto = M.to_proto t |> Writer.contents in
    try
      Reference.to_json ~proto_file ~message_type proto
    with
    | _ ->
      failwith "Could not parse reference json"
  in
  let test_json ?enum_names ?json_names ?omit_default_values t =
    let compare ~message t expect =
      match (M.from_json_exn expect = t) with
      | true -> ()
      | false ->
        let observed = M.from_json_exn expect |> json_ref in
        Printf.printf "Json encode/decode not identical. %s\n  Expect:  %s\n  Observe: %s\n" message
          (Yojson.Basic.to_string expect) (Yojson.Basic.to_string observed)
      | exception exn ->
        Printf.printf "Json encode/decode failed for %s: %s\n" message (Yojson.Basic.to_string expect);
        Printf.printf "  Error: %s\n" (Printexc.to_string exn);
    in
    let () =
      try
        let options = Json_options.make ?enum_names ?json_names ?omit_default_values () in
        let json = M.to_json options t in
        compare ~message:(M.name ()) t json
      with | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)
    in
    t
  in
  (* Compare reference json *)
  let () =
    try
      let json' = json_ref t in
      let t' = M.from_json_exn json' in
      let json = M.to_json Json_options.default t in
      let t'' =
        Reference.from_json ~proto_file ~message_type json
        |> Reader.create
        |> M.from_proto_exn
      in
      if t <> t' then Printf.printf "Deserialized json does not match.\n";
      if t <> t'' then Printf.printf "Deserialized generated json does not match\n";
      if (not (Yojson.Basic.equal json' json)) then
        Printf.printf "Generated json not equal\n";

      if (not (Yojson.Basic.equal json' json) || t <> t' || t <> t'' || debug) then
        Printf.printf "Json: %s\nRef:  %s\n"
          (Yojson.Basic.pretty_to_string json)
          (Yojson.Basic.pretty_to_string json');
    with
    | exn -> Printf.printf "Cannot deserialize reference json\n  Error: %s\n" (Printexc.to_string exn);
  in
  t
  |> test_json
  |> test_json ~enum_names:false
  |> test_json ~json_names:false
  |> test_json ~omit_default_values:false
  |> test_json ~enum_names:false ~json_names:false ~omit_default_values:false
  |> ignore

let test_decode (type t) (module M : T with type t = t) strategy expect data =
  let reader = Reader.create data in
  Test_runtime.set_stragegy strategy;
  match M.from_proto reader with
  | Ok observed -> begin
      match M.equal expect observed with
      | true -> ()
      | false ->
        Printf.printf "\n%s: Expect: %s\nObserved:%s\n" (Test_runtime.show_strategy strategy) ([%show: M.t] expect) ([%show: M.t] observed)
     end
  | Error err ->
    Printf.printf "\n%s:Decode failed: %s \n" (Test_runtime.show_strategy strategy) (Result.show_error err)
  | exception exn ->
    Reader.reset reader 0;
    let fields = Reader.to_list reader in
    Printf.printf "\n%s:Decode failed: %s\n" (Test_runtime.show_strategy strategy) (Printexc.to_string exn);
    Printf.printf "\n%s:Data: %s\n" (Test_runtime.show_strategy strategy) (List.map ~f:fst fields |> List.map ~f:string_of_int |> String.concat ~sep:", ")

(** Create a common function for testing. *)
let test_encode (type t) ?dump ?(debug_json=false) ?proto_file ?protoc_args (module M : T with type t = t) ?(skip_json=false) ?(skip_protoc=false) ?(validate : t option) ?(expect : t option) (t : t) =
  let expect = Option.value ~default:t expect in
  let () = match validate with
    | Some v when v <> expect -> Printf.printf "Validate match failed\n"
    | _ -> ()
  in
  let data = M.to_proto expect |> Writer.contents in
  let data_speed = let writer = Writer.init ~mode:Speed () in M.to_proto' writer expect; Writer.contents writer in
  let data_space = let writer = Writer.init ~mode:Space () in M.to_proto' writer expect; Writer.contents writer in
  let data_balanced = let writer = Writer.init ~mode:Balanced () in M.to_proto' writer expect; Writer.contents writer in

  let () =
    match dump with
    | Some _ -> hexlify data
    | None -> ()
  in
  let () =
    match proto_file with
    | Some proto_file when not skip_protoc ->
      let typename = M.name () in
      let typename = String.sub typename ~pos:1 ~len:(String.length typename - 1) in
      dump_protoc ?protoc_args ~proto_file typename data
    | _ -> ()
  in

  test_decode (module M) Test_runtime.Standard expect data_space;
  test_decode (module M) Test_runtime.Standard expect data_speed;
  test_decode (module M) Test_runtime.Standard expect data_balanced;
  test_decode (module M) Test_runtime.Standard expect data;
  test_decode (module M) Test_runtime.Fast expect data;
  test_decode (module M) Test_runtime.Full expect data;
  test_merge (module M) expect;
  match skip_json, proto_file with
  | false, Some proto_file ->
    test_json ~proto_file ~debug:debug_json (module M) expect;
  | _ -> ()
