[@@@ocaml.warning "-26"]
open Base
open Stdio

(*
let meassure = Bechamel_perf.Instance.cpu_clock
*)
let meassure = Bechamel.Toolkit.Instance. monotonic_clock

[@@@ocaml.warning "-32"]
module type Protoc_impl = sig
  type m
  val encode_pb_m: m -> Pbrt.Encoder.t -> unit
  val decode_pb_m: Pbrt.Decoder.t -> m
end

module type Plugin_impl = sig
  module M : Ocaml_protoc_plugin.Spec.Message
end

let make_tests name (type v) (module Protoc: Protoc_impl) (module Plugin: Plugin_impl with type M.t = v) v_plugin =

  (* Verify *)
  let verify_identity ~mode data =
    let writer = Ocaml_protoc_plugin.Writer.init ~mode () in
    Plugin.M.to_proto' writer data;
    let data' = Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create (Ocaml_protoc_plugin.Writer.contents writer)) in
    let () = match (Poly.equal data data') with
      | true -> ()
      | false ->
        failwith "Data not the same"
    in
    Ocaml_protoc_plugin.Writer.contents writer |> String.length,
    Ocaml_protoc_plugin.Writer.unused_space writer
  in
  let size_normal, unused_normal = verify_identity ~mode:Ocaml_protoc_plugin.Writer.Balanced v_plugin in
  let size_speed, unused_speed = verify_identity ~mode:Ocaml_protoc_plugin.Writer.Speed v_plugin in
  let size_space, unused_space = verify_identity ~mode:Ocaml_protoc_plugin.Writer.Space v_plugin in
  let data_plugin =
    let writer = Ocaml_protoc_plugin.Writer.init () in
    Plugin.M.to_proto' writer v_plugin;
    Ocaml_protoc_plugin.Writer.contents writer
  in
  let v_plugin' = Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create data_plugin) in
  assert (Poly.equal v_plugin v_plugin');
  let v_protoc = Protoc.decode_pb_m (Pbrt.Decoder.of_string data_plugin) in
  let protoc_encoder = Pbrt.Encoder.create () in
  let () = Protoc.encode_pb_m v_protoc protoc_encoder in
  let data_protoc = Pbrt.Encoder.to_string protoc_encoder in
  let v_plugin'' = Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create data_protoc) in
  let () = match Poly.equal v_plugin v_plugin'' with
    | true -> ()
    | false ->
       failwith "Data not the same"
  in
  printf "%-17s: %7d+%-7d(B) / %7d+%-7d(S) / %7d+%-7d(Sp) - %7d\n%!" name
    size_normal unused_normal size_speed unused_speed size_space unused_space (String.length data_protoc);


  let open Bechamel in
    let test_encode =
    Test.make_grouped ~name:"Encode"
      [
        Test.make ~name:"Plugin" (Staged.stage @@ fun () -> Plugin.M.to_proto' Ocaml_protoc_plugin.Writer.(init ()) v_plugin);
        Test.make ~name:"Protoc" (Staged.stage @@ fun () -> Protoc.encode_pb_m v_protoc (Pbrt.Encoder.create ()))
      ]
  in
  let test_decode =
    Test.make_grouped ~name:"Decode"
      [
        Test.make ~name:"Plugin" (Staged.stage @@ fun () -> Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create data_plugin));
        Test.make ~name:"Protoc" (Staged.stage @@ fun () -> Protoc.decode_pb_m (Pbrt.Decoder.of_string data_protoc))
      ]
  in
  Test.make_grouped ~name [test_encode; test_decode]

let make_int_tests vl =
  let open Ocaml_protoc_plugin in
  let open Bechamel in

  let make_group name =
    let name = Printf.sprintf "varint_%s(0x%08Lx)" name vl in
    Bechamel.Test.make_grouped ~name
  in

  let make_test id name ?reset f v =
    let f = match reset with
      | None -> (fun () -> f v)
      | Some reset -> (fun () -> f (reset v))
    in
    (* Reset is simple, but tests dont allow for this *)

    Test.make ~name:(Printf.sprintf "%s(0x%08Lx)" name id) (Staged.stage @@ f)
  in
  let v = Int64.to_int_exn vl in
  let buffer = Bytes.create 10 in
  let reader =
    let writer = Writer.init () in
    Writer.write_varint_value vl writer;
    Reader.create (Writer.contents writer)
  in
  let pbrt_buf = Pbrt.Encoder.create ~size:10 () in
  let make_pbrt_reader =
    let encoder = Pbrt.Encoder.create () in
    Pbrt.Encoder.int64_as_varint vl encoder;
    let buffer = Pbrt.Encoder.to_bytes encoder in
    fun _ -> Pbrt.Decoder.of_bytes buffer
  in
  [
    make_group "boxed" [
      make_test vl "Read"  ~reset:(fun reader -> Reader.reset reader 0; reader) Reader.read_varint reader;
      make_test vl "Read_pbrt" ~reset:make_pbrt_reader Pbrt.Decoder.int64_as_varint (Pbrt.Decoder.of_string "") ;
      make_test vl "Write" (Writer.write_varint buffer ~offset:0) vl;
      make_test vl "Write_pbrt" ~reset:(fun writer -> Pbrt.Encoder.reset writer; writer) (Pbrt.Encoder.int64_as_varint vl) pbrt_buf;

    ];
    make_group "unboxed" [
      make_test vl "Read" ~reset:(fun reader -> Reader.reset reader 0; reader) Reader.read_varint_unboxed reader;
      make_test vl "Read_pbrt" ~reset:make_pbrt_reader Pbrt.Decoder.int_as_varint (Pbrt.Decoder.of_string "") ;
      make_test vl "Write" (Writer.write_varint_unboxed buffer ~offset:0) v;
      make_test vl "Write_pbrt" ~reset:(fun writer -> Pbrt.Encoder.reset writer; writer) (Pbrt.Encoder.int_as_varint v) pbrt_buf;
    ]
  ]

let _ =
  Random.init 0;
  let module Gc = Stdlib.Gc in
  Gc.full_major ();
  let control = Gc.get () in
  Gc.set { control with minor_heap_size=4000_1000; space_overhead=500 }


let random_list ~len ~f () =
  List.init len ~f:(fun _ -> f ())

let random_string ~len () =
  String.init len ~f:(fun _ -> Random.char ())

let create_test_data ~depth () =
  let module M = Plugin.Bench.M in
  let module Data = Plugin.Bench.Data in
  let module Enum = Plugin.Bench.Enum in
  let optional ~f () =
    match (Random.int 4 = 0) with
    | true -> None
    | false -> Some (f ())
  in
  let create_data () =

    let random_enum () =
      Array.random_element_exn [| Enum.EA; Enum.EB; Enum.EC; Enum.ED; Enum.EE; |]
    in
    let s1 = random_string ~len:20 () in
    let n1 = random_list ~len:100 ~f:(fun () -> Random.int 1_000) () in
    let n2 = random_list ~len:100 ~f:(fun () -> Random.int 1_000) () in
    let d1 = random_list ~len:100 ~f:(fun () -> Random.float 1_000.) () in
    let n3 = Random.int 10 in
    let b1 = Random.bool () in
    let e = random_list ~len:100 ~f:random_enum () in

    Data.make ~s1 ~n1 ~n2 ~d1 ~n3 ~b1 (* ~e *) ()
  in

  let rec create_btree n () =
    match n with
    | 0 -> None
    | n ->
      let data = random_list ~len:2 ~f:create_data () in
      let children =
        random_list ~len:2 ~f:(create_btree (n - 1)) () |> List.filter_opt
      in
      M.make ~children ~data () |> Option.some
  in
  create_btree depth ()

let time = 5.0
let benchmark tests =
  let open Bechamel in
  let instances = [ meassure ] in
  let cfg = Benchmark.cfg ~compaction:false ~kde:(Some 1) ~quota:(Time.second time) () in
  Benchmark.all cfg instances tests

let analyze results =
  let open Bechamel in
  let ransac = Analyze.ransac ~filter_outliers:true ~predictor:Measure.run in
  let ols = Analyze.ols ~bootstrap:5 ~r_square:true ~predictors:[| Measure.run |] in
  let results = Analyze.all ransac meassure results in
  ignore (ransac, ols);
  Analyze.merge ransac [ meassure ] [ results ]

let print_results: (string, (string, Bechamel.Analyze.RANSAC.t) Stdlib.Hashtbl.t) Stdlib.Hashtbl.t -> unit = fun results ->
  let module R = Bechamel.Analyze.RANSAC in
  let open Stdlib in
  Hashtbl.to_seq results |> List.of_seq |> List.sort (fun a b -> String.compare (fst a) (fst b))
  |> List.iter (fun (key, data) ->
    Hashtbl.to_seq data |> List.of_seq |> List.sort (fun a b -> String.compare (fst a) (fst b))
    |> function
    | [name, decode_plugin; _, decode_protoc; _, encode_plugin; _, encode_protoc] ->
      let name = String.split_on_char '/' name |> List.hd in
      printf "| %20s/decode | %10.2f | %10.2f |  %2.2f |\n" name (R.mean decode_plugin) (R.mean decode_protoc) ((R.mean decode_plugin)/.(R.mean decode_protoc));
      printf "| %20s/encode | %10.2f | %10.2f |  %2.2f |\n" name (R.mean encode_plugin) (R.mean encode_protoc) ((R.mean encode_plugin)/.(R.mean encode_protoc));
    | data ->
      printf "New group\n";
      List.iter (fun (name, data) ->
        printf "| %30s | %8.2f | | |\n" name (R.mean data)
      ) data
  )

let _ =
  let v_plugin = create_test_data ~depth:4 () in
  let v_plugin = Option.value_exn v_plugin in
  let tests =
    [
      make_tests "bench" (module Protoc.Bench) (module Plugin.Bench) v_plugin;
      make_tests "int64" (module Protoc.Int64) (module Plugin.Int64) 27;
      make_tests "float" (module Protoc.Float) (module Plugin.Float) 27.0001;
      make_tests "string" (module Protoc.String) (module Plugin.String) "Benchmark";
      make_tests "enum" (module Protoc.Enum) (module Plugin.Enum) Plugin.Enum.Enum.ED;
      make_tests "empty" (module Protoc.Empty) (module Plugin.Empty) ();

      List.init 1000 ~f:(fun i -> i) |> make_tests "int64 list" (module Protoc.Int64_list) (module Plugin.Int64_list);
      List.init 1000 ~f:(fun i -> Float.of_int i) |> make_tests "float list" (module Protoc.Float_list) (module Plugin.Float_list);
      List.init 1000 ~f:(fun _ -> random_string ~len:20 ()) |> make_tests "string list" (module Protoc.String_list) (module Plugin.String_list);
      (* Proto plugin requires all fields to be present in a map *)
      List.init 1000 ~f:(fun i -> i+1, i * i+1) |> make_tests "map<int64,int64>" (module Protoc.Map) (module Plugin.Map);

      (* random_list ~len:100 ~f:(fun () -> Plugin.Enum_list.Enum.ED) () |> make_tests (module Protoc.Enum_list) (module Plugin.Enum_list); *)
    ] @ make_int_tests (0xFFFF_FFFFL) @ make_int_tests (0xFFFF_FFFF_FFFF_FFFFL)
  in
  printf "|          Name               |   plugin   |   protoc   | ratio |\n";
  printf "|           --                |     --     |     --     |  --   |\n";
  List.iter ~f:(fun test ->
    test
    |> benchmark
    |> analyze
    |> print_results
  ) tests
