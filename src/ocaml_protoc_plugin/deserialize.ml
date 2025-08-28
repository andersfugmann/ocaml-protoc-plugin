(** Module for deserializing values *)
open StdLabels
open Spec

(** Exception indicating that fast deserialization did not succeed and revert to full deserialization *)
exception Restart_full

type 'a reader = 'a -> Reader.t -> Field.field_type -> 'a
type ('a, 'b) getter = 'a -> 'b
type 'a field_spec = (int * 'a reader)
type _ value = Value: ('b field_spec list * 'b * ('b, 'a) getter) -> 'a value [@@unboxed]
type extensions = (int * Field.t) list

type (_, _) value_list =
  | VNil : ('a, 'a) value_list
  | VNil_ext : (extensions -> 'a, 'a) value_list
  | VCons : 'a value * ('b, 'c) value_list -> ('a -> 'b, 'c) value_list

type sentinel_field_spec = int * (Reader.t -> Field.field_type -> unit)
type 'a sentinel_getter = unit -> 'a

type (_, _) sentinel_list =
  | NNil : ('a, 'a) sentinel_list
  | NNil_ext: (extensions -> 'a, 'a) sentinel_list
  | NCons : (sentinel_field_spec list  * 'a sentinel_getter) * ('b, 'c) sentinel_list -> ('a -> 'b, 'c) sentinel_list

let error_wrong_field str field = Result.raise (`Wrong_field_type (str, Field.show field))
let error_required_field_missing index spec = Result.raise (`Required_field_missing (index, Spec.show spec))

let decode_zigzag v =
  let open Infix.Int64 in
  match v land 0x01L = 0L with
  | true -> v lsr 1
  | false -> (v lsr 1 * -1L) - 1L

let decode_zigzag_unboxed v =
  match v land 0x01 = 0 with
  | true -> v lsr 1
  | false -> (v lsr 1 * -1) - 1

let int_of_uint32 =
  let open Int32 in
  match Sys.word_size with
  | 32 -> Int32.to_int
  | 64 ->
      let mask = 0xFFFF lsl 16 lor 0xFFFF in
      fun n -> (to_int n land mask)
  | _ ->
    assert false

let read_fixed64 ~f reader = Reader.read_fixed64 reader |> f
let read_fixed32 ~f reader = Reader.read_fixed32 reader |> f
let read_varint_unboxed ~f reader = Reader.read_varint_unboxed reader |> f
let read_varint ~f reader = Reader.read_varint reader |> f

let (@@) f g = fun v -> f v |> g

let read_of_spec: type a b. (a, b) spec -> Field.field_type * (Reader.t -> a) = function
  | Double -> Fixed64, read_fixed64 ~f:Int64.float_of_bits
  | Float  -> Fixed32, read_fixed32 ~f:Int32.float_of_bits
  | Int32 -> Varint, read_varint_unboxed ~f:Int32.of_int
  | Int32_int -> Varint, Reader.read_varint_unboxed
  | Int64 -> Varint, Reader.read_varint
  | Int64_int -> Varint, Reader.read_varint_unboxed
  | UInt32 -> Varint, read_varint_unboxed ~f:Int32.of_int
  | UInt32_int -> Varint, Reader.read_varint_unboxed
  | UInt64 -> Varint, Reader.read_varint
  | UInt64_int -> Varint, Reader.read_varint_unboxed
  | SInt32 -> Varint, read_varint_unboxed ~f:(decode_zigzag_unboxed @@ Int32.of_int)
  | SInt32_int -> Varint, read_varint_unboxed ~f:decode_zigzag_unboxed
  | SInt64 -> Varint, read_varint ~f:decode_zigzag
  | SInt64_int -> Varint, read_varint_unboxed ~f:decode_zigzag_unboxed

  | Fixed32 -> Fixed32, Reader.read_fixed32
  | Fixed32_int -> Fixed32, read_fixed32 ~f:int_of_uint32
  | SFixed32 -> Fixed32, Reader.read_fixed32
  | SFixed32_int -> Fixed32, read_fixed32 ~f:Int32.to_int

  | Fixed64 -> Fixed64, Reader.read_fixed64
  | Fixed64_int -> Fixed64, read_fixed64 ~f:Int64.to_int
  | SFixed64 -> Fixed64, Reader.read_fixed64
  | SFixed64_int -> Fixed64, read_fixed64 ~f:Int64.to_int

  | Bool -> Varint, fun reader -> Reader.read_varint_unboxed reader != 0
  | Enum (module Enum) -> Varint, read_varint_unboxed ~f:Enum.from_int_exn
  | String -> Length_delimited, fun reader ->
    let Field.{ offset; length; data } = Reader.read_length_delimited reader in
    String.sub ~pos:offset ~len:length data
  | Bytes -> Length_delimited, fun reader ->
    let Field.{ offset; length; data } = Reader.read_length_delimited reader in
    let v = Bytes.create length in
    String.unsafe_blit ~src:data ~src_pos:offset ~dst:v ~dst_pos:0 ~len:length;
    v
  | Message (module Message) -> Length_delimited, fun reader ->
    let Field.{ offset; length; data } = Reader.read_length_delimited reader in
    Message.from_proto_exn (Reader.create ~offset ~length data)

let id x = x
let keep_last _ v = v
let merge_opt merge v1 v2 =
  match v1 with
  | None -> Some v2
  | Some v1 -> Some (merge v1 v2)
let keep_last_opt _ v = Some v

let read_field ~read:(expect, read_f) ~map v reader field_type =
  match expect = field_type with
  | true -> read_f reader |> map v
  | false ->
    let field = Reader.read_field_content field_type reader in
    error_wrong_field "Deserialize" field

let read_map_entry: type a b. read_key:a value -> read_value:b value -> Reader.t -> (a * b) = fun ~read_key ~read_value ->
  let Value (key_field_specs, default_key, get_key) = read_key in
  let Value (value_field_specs, default_value, get_value) = read_value in
  let (key_index, read_key_f) = List.hd key_field_specs in
  let (value_index, read_value_f) = List.hd value_field_specs in
  let rec read (key, value) reader =
    match Reader.has_more reader with
    | true -> begin
        match Reader.read_field_header reader with
        | field_type, index when index = key_index ->
          let key = read_key_f key reader field_type in
          read (key, value) reader
        | field_type, index when index = value_index ->
          let value = read_value_f value reader field_type in
          read (key, value) reader
        | field_type, _ ->
          Reader.read_field_content field_type reader |> ignore;
          read (key, value) reader (* We should drop the value *)
      end
    | false -> (get_key key, get_value value)
  in
  read (default_key, default_value)

let rec value: type a b. (a, b) compound -> a value = function
  | Basic_req ((index, _, _), spec) ->
    let read = read_field ~read:(read_of_spec spec) ~map:keep_last_opt in
    let getter = function Some v -> v | None -> error_required_field_missing index spec in
    Value ([(index, read)], None, getter)
  | Basic ((index, _, _), spec, default) ->
    let read = read_field ~read:(read_of_spec spec) ~map:keep_last in
    Value ([(index, read)], default, id)
  | Basic_opt ((index, _, _), spec) ->
    let map = match spec with
      | Message (module Message) -> merge_opt Message.merge
      | _ -> keep_last_opt
    in
    let read = read_field ~read:(read_of_spec spec) ~map in
    Value ([(index, read)], None, id)
  | Repeated ((index, _, _), spec, Packed) ->
    let field_type, read_f = read_of_spec spec in
    let rec read_packed_values read_f acc reader =
      match Reader.has_more reader with
      | true -> read_packed_values read_f (read_f reader :: acc) reader
      | false -> acc
    in
    let read vs reader = fun (ft : Field.field_type) -> match ft with
      | Field.Length_delimited ->
        let Field.{ offset; length; data } = Reader.read_length_delimited reader in
          let reader = Reader.create ~offset ~length data in
          read_packed_values read_f vs reader
      | ft when ft = field_type ->
        read_f reader :: vs
      | ft ->
        let field = Reader.read_field_content ft reader in
        error_wrong_field "Deserialize" field
    in
    Value ([(index, read)], [], List.rev)
  | Repeated ((index, _, _), spec, Not_packed) ->
    let read = read_field ~read:(read_of_spec spec) ~map:(fun vs v -> v :: vs) in
    Value ([(index, read)], [], List.rev)
  | Map ((index, _, _), (key_spec, value_compound)) ->
    let read_key = value (Basic ((1, "key", "key"), key_spec, default_of_spec key_spec)) in
    let read_value = value value_compound in
    let read_entry = read_map_entry ~read_key ~read_value in

    let read_entry_message reader =
      let Field.{ offset; length; data } = Reader.read_length_delimited reader in
      read_entry (Reader.create ~offset ~length data)
    in
    let read = read_field ~read:(Field.Length_delimited, read_entry_message) ~map:(fun vs v -> v :: vs) in
    Value ([(index, read)], [], List.rev)
  | Oneof (oneofs, _index_f) ->
    let make_reader: a oneof -> a field_spec = fun (Oneof_elem ((index, _, _), spec, (constr, _destr))) ->
      let read = read_field ~read:(read_of_spec spec) ~map:(fun _ -> constr) in
      (index, read)
    in
    Value (List.map ~f:make_reader oneofs, `not_set, id)

module IntMap = Map.Make(struct type t = int let compare = Int.compare end)

let rec extension_ranges: type a b. (a, b) compound_list -> extension_ranges = function
  | Nil -> []
  | Nil_ext extension_ranges -> extension_ranges
  | Cons (_, rest) -> extension_ranges rest

let in_extension_ranges extension_ranges index =
  List.exists ~f:(fun (start, end') -> index >= start && index <= end') extension_ranges

let rec make_values: type a b. (a, b) compound_list -> (a, b) value_list = function
  | Nil -> VNil
  | Nil_ext _extension_ranges -> VNil_ext
  | Cons (spec, rest) ->
    let value = value spec in
    let values = make_values rest in
    VCons (value, values)

(** Full (slow) deserialization. *)
let deserialize_full: type constr a. extension_ranges -> (constr, a) value_list -> constr -> Reader.t -> a  = fun extension_ranges values constructor reader ->
  let rec make_sentinel_list: type a b. (a, b) value_list -> (a, b) sentinel_list = function
    | VNil -> NNil
    | VNil_ext -> NNil_ext
    | VCons (Value (fields, default, getter), rest) ->
      let v = ref default in
      let get () = getter !v in
      let fields =
        List.map ~f:(fun (index, read) ->
          let read reader field_type = (v := read !v reader field_type) in
          (index, read)
        ) fields
      in
      NCons ((fields, get), make_sentinel_list rest)
  in

  let rec create_map: type a b. _ IntMap.t -> (a, b) sentinel_list -> _ IntMap.t = fun map -> function
    | NNil -> map
    | NNil_ext -> map
    | NCons ((fields, _), rest) ->
      let map =
        List.fold_left ~init:map ~f:(fun map (index, read)-> IntMap.add index read map) fields
      in
      create_map map rest
  in

  let rec apply: type constr a. extensions -> constr -> (constr, a) sentinel_list -> a = fun extensions constr -> function
    | NNil -> constr
    | NNil_ext -> constr extensions
    | NCons ((_, get), rest) ->
      apply extensions (constr (get ())) rest
  in

  let rec read: (Reader.t -> Field.field_type -> unit) IntMap.t -> extensions -> extensions = fun map extensions ->
    match Reader.has_more reader with
    | false -> List.rev extensions
    | true ->
      let (field_type, field_number) = Reader.read_field_header reader in
      match IntMap.find_opt field_number map with
      | Some read_f ->
        read_f reader field_type;
        read map extensions
      | None when in_extension_ranges extension_ranges field_number ->
        let field = Reader.read_field_content field_type reader in
        read map ((field_number, field) :: extensions)
      | None ->
        let (_: Field.t) = Reader.read_field_content field_type reader in
        read map extensions
  in
  let sentinels = make_sentinel_list values in
  let map = create_map IntMap.empty sentinels in
  let extensions = read map [] in
  apply extensions constructor sentinels

(* Fast deserialization. Assumes all fields in input are sorted. *)
let deserialize_fast: type constr a. extension_ranges -> (constr, a) value_list -> constr -> Reader.t -> a  = fun extension_ranges values constr reader ->

  let rec read_fields: type a. extension_ranges -> Field.field_type -> int -> Reader.t -> extensions -> a -> (int * a reader) list -> ((Field.field_type * int) option * extensions * a) = fun extension_ranges tpe idx reader extensions v -> function
    | ((index, read_f) :: _) as lst when idx = index ->
      let v = read_f v reader tpe in
      begin
        match Reader.next_field_header reader with
        | Some (tpe, idx) -> read_fields extension_ranges tpe idx reader extensions v lst
        | None -> (None, extensions, v)
      end
    | rest when in_extension_ranges extension_ranges idx ->
      let extensions = (idx, Reader.read_field_content tpe reader) :: extensions in
      begin
        match Reader.next_field_header reader with
        | Some (tpe, idx) -> read_fields extension_ranges tpe idx reader extensions v rest
        | None -> (None, extensions, v)
      end
    | _ :: rest -> read_fields extension_ranges tpe idx reader extensions v rest
    | [] -> (Some (tpe, idx), extensions, v)
  in

  let rec apply: type constr a. constr -> extensions -> (constr, a) value_list -> a = fun constr extensions -> function
    | VNil -> constr
    | VNil_ext -> constr (List.rev extensions)
    | VCons (Value (_, default, get), vs) -> apply (constr (get default)) extensions vs
  in

  let rec read_values: type constr a. extension_ranges -> (Field.field_type * int) option -> Reader.t -> constr -> extensions -> (constr, a) value_list -> a = fun extension_ranges next_field reader constr extensions vs ->
    match next_field with
    | None -> apply constr extensions vs
    | Some (tpe, idx) -> match vs with
      | VCons (Value (fields, default, get), vs) ->
        let (next_field, extensions, v) = read_fields extension_ranges tpe idx reader extensions default fields in
        read_values extension_ranges next_field reader (constr (get v)) extensions vs
      | VNil | VNil_ext ->
        (* This implies that there are still fields to be read.
           Revert to full deserialization.
        *)
        raise Restart_full
  in
  let next_field = Reader.next_field_header reader in
  read_values extension_ranges next_field reader constr [] values

let deserialize: type constr a. (constr, a) compound_list -> constr -> Reader.t -> a = fun spec constr ->
  let extension_ranges = extension_ranges spec in
  let values = make_values spec in
  fun reader ->
    let offset = Reader.offset reader in
    try
      deserialize_fast extension_ranges values constr reader
    with (Restart_full | Result.Error `Required_field_missing _ ) ->
      (* Revert to full deserialization *)
      Reader.reset reader offset;
      deserialize_full extension_ranges values constr reader

let deserialize_full: type constr a. (constr, a) compound_list -> constr -> Reader.t -> a = fun spec constr ->
  let extension_ranges = extension_ranges spec in
  let values = make_values spec in
  fun reader -> deserialize_full extension_ranges values constr reader

let deserialize_fast: type constr a. (constr, a) compound_list -> constr -> Reader.t -> a = fun spec constr ->
  let extension_ranges = extension_ranges spec in
  let values = make_values spec in
  fun reader -> deserialize_fast extension_ranges values constr reader

let%expect_test "zigzag decoding" =
  let n2l = Int64.of_int in
  let i2l = Int64.of_int32 in
  let test_values =
    [Int64.min_int; n2l Int.min_int; i2l Int32.min_int; -2L;
     0L; 3L; i2l Int32.max_int; n2l Int.max_int; Int64.max_int]
    |> List.map ~f:(
      let open Infix.Int64 in
      function
      | v when v > 0L -> [pred v; v]
      | v -> [v; succ v]
    )
    |> List.concat
  in
  List.iter ~f:(fun vl -> Printf.printf "zigzag_decoding(0x%016Lx) = 0x%016Lx\n" vl (decode_zigzag vl)) test_values;
  List.iter ~f:(fun v -> Printf.printf "zigzag_decoding_unboxed(0x%016x) = 0x%016x\n" (Int64.to_int v) (Int64.to_int v |> decode_zigzag_unboxed)) test_values;
  (* The results should display alternation between positive and negative numbers. If the right most bit is set, the number is negative *)
  [%expect {|
    zigzag_decoding(0x8000000000000000) = 0x4000000000000000
    zigzag_decoding(0x8000000000000001) = 0xbfffffffffffffff
    zigzag_decoding(0xc000000000000000) = 0x6000000000000000
    zigzag_decoding(0xc000000000000001) = 0x9fffffffffffffff
    zigzag_decoding(0xffffffff80000000) = 0x7fffffffc0000000
    zigzag_decoding(0xffffffff80000001) = 0x800000003fffffff
    zigzag_decoding(0xfffffffffffffffe) = 0x7fffffffffffffff
    zigzag_decoding(0xffffffffffffffff) = 0x8000000000000000
    zigzag_decoding(0x0000000000000000) = 0x0000000000000000
    zigzag_decoding(0x0000000000000001) = 0xffffffffffffffff
    zigzag_decoding(0x0000000000000002) = 0x0000000000000001
    zigzag_decoding(0x0000000000000003) = 0xfffffffffffffffe
    zigzag_decoding(0x000000007ffffffe) = 0x000000003fffffff
    zigzag_decoding(0x000000007fffffff) = 0xffffffffc0000000
    zigzag_decoding(0x3ffffffffffffffe) = 0x1fffffffffffffff
    zigzag_decoding(0x3fffffffffffffff) = 0xe000000000000000
    zigzag_decoding(0x7ffffffffffffffe) = 0x3fffffffffffffff
    zigzag_decoding(0x7fffffffffffffff) = 0xc000000000000000
    zigzag_decoding_unboxed(0x0000000000000000) = 0x0000000000000000
    zigzag_decoding_unboxed(0x0000000000000001) = 0x7fffffffffffffff
    zigzag_decoding_unboxed(0x4000000000000000) = 0x2000000000000000
    zigzag_decoding_unboxed(0x4000000000000001) = 0x5fffffffffffffff
    zigzag_decoding_unboxed(0x7fffffff80000000) = 0x3fffffffc0000000
    zigzag_decoding_unboxed(0x7fffffff80000001) = 0x400000003fffffff
    zigzag_decoding_unboxed(0x7ffffffffffffffe) = 0x3fffffffffffffff
    zigzag_decoding_unboxed(0x7fffffffffffffff) = 0x4000000000000000
    zigzag_decoding_unboxed(0x0000000000000000) = 0x0000000000000000
    zigzag_decoding_unboxed(0x0000000000000001) = 0x7fffffffffffffff
    zigzag_decoding_unboxed(0x0000000000000002) = 0x0000000000000001
    zigzag_decoding_unboxed(0x0000000000000003) = 0x7ffffffffffffffe
    zigzag_decoding_unboxed(0x000000007ffffffe) = 0x000000003fffffff
    zigzag_decoding_unboxed(0x000000007fffffff) = 0x7fffffffc0000000
    zigzag_decoding_unboxed(0x3ffffffffffffffe) = 0x1fffffffffffffff
    zigzag_decoding_unboxed(0x3fffffffffffffff) = 0x6000000000000000
    zigzag_decoding_unboxed(0x7ffffffffffffffe) = 0x3fffffffffffffff
    zigzag_decoding_unboxed(0x7fffffffffffffff) = 0x4000000000000000
    |}]
