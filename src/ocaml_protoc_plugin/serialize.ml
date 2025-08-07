open StdLabels

open Spec

let field_type: type a b. (a, b) spec -> int = function
  | Int64 | UInt64 | SInt64 | Int32 | UInt32 | SInt32
  | Int64_int | UInt64_int | Int32_int | UInt32_int | SInt64_int | SInt32_int
  | Bool | Enum _ -> 0 (* Varint *)
  | String | Bytes | Message _ -> 2 (* Length delimited *)
  | Double | Fixed64 | SFixed64 | Fixed64_int | SFixed64_int -> 1 (* Fixed 64 bit *)
  | Float | Fixed32 | SFixed32 | Fixed32_int | SFixed32_int -> 5 (* Fixed 32 bit *)

let write_fixed64 ~f v =
  Writer.write_fixed64_value (f v)

let write_fixed32 ~f v =
  Writer.write_fixed32_value (f v)

let encode_zigzag v =
  let open Infix.Int64 in
  let v = match v < 0L with
    | true -> v lsl 1 lxor (-1L)
    | false -> v lsl 1
  in
  v

let encode_zigzag_unboxed v =
  let v = match v < 0 with
    | true -> v lsl 1 lxor (-1)
    | false -> v lsl 1
  in
  v

let write_varint ~f v = Writer.write_varint_value (f v)

let write_varint_unboxed ~f v =
  Writer.write_varint_unboxed_value (f v)

let write_length_delimited_string ~f v =
  let v = f v in
  Writer.write_length_delimited_value ~data:v ~offset:0 ~len:(String.length v)

let (@@) a b = fun v -> b (a v)

let write_value : type a b. (a, b) spec -> a -> Writer.t -> unit = function
  | Double -> write_fixed64 ~f:Int64.bits_of_float
  | Float -> write_fixed32 ~f:Int32.bits_of_float
  | Fixed64 -> Writer.write_fixed64_value
  | SFixed64 -> Writer.write_fixed64_value
  | Fixed64_int -> write_fixed64 ~f:Int64.of_int
  | SFixed64_int -> write_fixed64 ~f:Int64.of_int
  | Fixed32 -> Writer.write_fixed32_value
  | SFixed32 -> Writer.write_fixed32_value
  | Fixed32_int -> write_fixed32 ~f:Int32.of_int
  | SFixed32_int -> write_fixed32 ~f:Int32.of_int
  | Int64 -> Writer.write_varint_value
  | UInt64 -> Writer.write_varint_value
  | SInt64 -> write_varint ~f:encode_zigzag
  | Int32 -> write_varint_unboxed ~f:Int32.to_int
  | UInt32 -> write_varint_unboxed ~f:Int32.to_int
  | SInt32 -> write_varint_unboxed ~f:(Int32.to_int @@ encode_zigzag_unboxed)
  | Int64_int -> Writer.write_varint_unboxed_value
  | UInt64_int -> Writer.write_varint_unboxed_value
  | Int32_int -> Writer.write_varint_unboxed_value
  | UInt32_int -> Writer.write_varint_unboxed_value
  | SInt64_int -> write_varint_unboxed ~f:encode_zigzag_unboxed
  | SInt32_int -> write_varint_unboxed ~f:encode_zigzag_unboxed

  | Bool -> write_varint_unboxed ~f:(function true -> 1 | false -> 0)
  | String -> fun v -> Writer.write_length_delimited_value ~data:v ~offset:0 ~len:(String.length v)
  | Bytes -> write_length_delimited_string ~f:Bytes.unsafe_to_string
  | Enum (module Enum) -> write_varint_unboxed ~f:Enum.to_int
  | Message (module Message) ->
    Writer.write_length_delimited_f ~write_f:Message.to_proto'

(** Optimized when the value is given in advance, and the continuation is expected to be called multiple times *)
let write_value_const : type a b. (a, b) spec -> a -> Writer.t -> unit = fun spec v ->
  let write_value = write_value spec in
  let writer = Writer.init () in
  write_value v writer;
  let data = Writer.contents writer in
  Writer.write_const_value data

let write_field_header: _ spec -> int -> Writer.t -> unit = fun spec index ->
  let field_type = field_type spec in
  let header = (index lsl 3) + field_type in
  write_value_const Int64_int header

let write_field: type a b. (a, b) spec -> int -> Writer.t -> a -> unit = fun spec index ->
  let write_field_header = write_field_header spec index in
  let write_value = write_value spec in
  fun writer v->
    write_field_header writer;
    write_value v writer

let rec write: type a b. (a, b) compound -> Writer.t -> a -> unit = function
  | Repeated ((index, _, _), spec, Packed) -> begin
      let write_value = write_value spec in
      let write_f writer vs = List.iter ~f:(fun v -> write_value v writer) vs in
      let write_header = write_field_header String index in
      fun writer vs ->
        match vs with
        | [] -> ()
        | vs ->
          write_header writer;
          Writer.write_length_delimited_f ~write_f vs writer
    end
  | Repeated ((index, _, _), spec, Not_packed) ->
    let write = write_field spec index in
    fun writer vs ->
      List.iter ~f:(fun v -> write writer v) vs
  | Map ((index, _, _), (key_spec, value_compound)) ->
    let write_header = write_field_header String index in
    let write_key = write (Basic_req ((1, "key", "key"), key_spec)) in
    let write_value = write value_compound in
    let write_entry writer (key, value) =
      write_key writer key;
      write_value writer value;
      ()
    in
    let write = Writer.write_length_delimited_f ~write_f:write_entry in
    fun writer vs ->
      List.iter ~f:(fun v ->
        write_header writer;
        write v writer
      ) vs
  | Basic ((index, _, _), spec, default) -> begin
      let write = write_field spec index in
      let writer writer = function
        | v when v = default -> ()
        | v -> write writer v
      in
      writer
    end
  | Basic_req ((index, _, _), spec) ->
      write_field spec index
  | Basic_opt ((index, _, _), spec) -> begin
      let write = write_field spec index in
      fun writer v ->
        match v with
        | Some v -> write writer v
        | None -> ()
  end
  | Oneof (oneofs, index_f) -> begin
      let create_writer: type a. a oneof -> (Writer.t -> a -> unit) = function
        | Oneof_elem (field, spec, (_constr, destructor)) ->
          let write = write (Basic_req (field, spec)) in
          fun writer v ->
            write writer (destructor v)
      in
      let field_writers = List.map ~f:create_writer oneofs |> Array.of_list in
      fun writer -> function
        | `not_set -> ()
        | v ->
          let index = index_f v in
          let write = Array.unsafe_get field_writers index in
          write writer v
    end

let in_extension_ranges extension_ranges index =
  List.exists ~f:(fun (start, end') -> index >= start && index <= end') extension_ranges

let rec serialize: type a. (a, unit) compound_list -> Writer.t -> a = function
  | Nil -> fun _writer -> ()
  | Nil_ext extension_ranges ->
    fun writer extensions ->
      List.iter ~f:(function
        | (index, field) when in_extension_ranges extension_ranges index -> Writer.write_field writer index field
        | _ -> ()
      ) extensions;
      ()
  | Cons (compound, rest) ->
    let cont = serialize rest in
    let write = write compound in
    fun writer v ->
      write writer v;
      cont writer

let%expect_test "zigzag encoding" =
  let n2l = Int64.of_int in
  let i2l = Int64.of_int32 in
  let test_values =
    [Int64.min_int; n2l Int.min_int; i2l Int32.min_int; -2L;
     0L; 3L; i2l Int32.max_int; n2l Int.max_int; Int64.max_int]
    |> List.concat_map ~f:(
      let open Infix.Int64 in
      function
      | v when v > 0L -> [pred v; v]
      | v -> [v; succ v]
    )
  in
  List.iter ~f:(fun vl -> Printf.printf "zigzag_encoding 0x%016Lx = 0x%016Lx\n" vl (encode_zigzag vl)) test_values;
  List.iter ~f:(fun v -> Printf.printf "zigzag_encoding_unboxed 0x%016x = 0x%016x\n" (Int64.to_int v) (Int64.to_int v |> encode_zigzag_unboxed)) test_values;
  [%expect {|
    zigzag_encoding 0x8000000000000000 = 0xffffffffffffffff
    zigzag_encoding 0x8000000000000001 = 0xfffffffffffffffd
    zigzag_encoding 0xc000000000000000 = 0x7fffffffffffffff
    zigzag_encoding 0xc000000000000001 = 0x7ffffffffffffffd
    zigzag_encoding 0xffffffff80000000 = 0x00000000ffffffff
    zigzag_encoding 0xffffffff80000001 = 0x00000000fffffffd
    zigzag_encoding 0xfffffffffffffffe = 0x0000000000000003
    zigzag_encoding 0xffffffffffffffff = 0x0000000000000001
    zigzag_encoding 0x0000000000000000 = 0x0000000000000000
    zigzag_encoding 0x0000000000000001 = 0x0000000000000002
    zigzag_encoding 0x0000000000000002 = 0x0000000000000004
    zigzag_encoding 0x0000000000000003 = 0x0000000000000006
    zigzag_encoding 0x000000007ffffffe = 0x00000000fffffffc
    zigzag_encoding 0x000000007fffffff = 0x00000000fffffffe
    zigzag_encoding 0x3ffffffffffffffe = 0x7ffffffffffffffc
    zigzag_encoding 0x3fffffffffffffff = 0x7ffffffffffffffe
    zigzag_encoding 0x7ffffffffffffffe = 0xfffffffffffffffc
    zigzag_encoding 0x7fffffffffffffff = 0xfffffffffffffffe
    zigzag_encoding_unboxed 0x0000000000000000 = 0x0000000000000000
    zigzag_encoding_unboxed 0x0000000000000001 = 0x0000000000000002
    zigzag_encoding_unboxed 0x4000000000000000 = 0x7fffffffffffffff
    zigzag_encoding_unboxed 0x4000000000000001 = 0x7ffffffffffffffd
    zigzag_encoding_unboxed 0x7fffffff80000000 = 0x00000000ffffffff
    zigzag_encoding_unboxed 0x7fffffff80000001 = 0x00000000fffffffd
    zigzag_encoding_unboxed 0x7ffffffffffffffe = 0x0000000000000003
    zigzag_encoding_unboxed 0x7fffffffffffffff = 0x0000000000000001
    zigzag_encoding_unboxed 0x0000000000000000 = 0x0000000000000000
    zigzag_encoding_unboxed 0x0000000000000001 = 0x0000000000000002
    zigzag_encoding_unboxed 0x0000000000000002 = 0x0000000000000004
    zigzag_encoding_unboxed 0x0000000000000003 = 0x0000000000000006
    zigzag_encoding_unboxed 0x000000007ffffffe = 0x00000000fffffffc
    zigzag_encoding_unboxed 0x000000007fffffff = 0x00000000fffffffe
    zigzag_encoding_unboxed 0x3ffffffffffffffe = 0x7ffffffffffffffc
    zigzag_encoding_unboxed 0x3fffffffffffffff = 0x7ffffffffffffffe
    zigzag_encoding_unboxed 0x7ffffffffffffffe = 0x0000000000000003
    zigzag_encoding_unboxed 0x7fffffffffffffff = 0x0000000000000001
    |}]
