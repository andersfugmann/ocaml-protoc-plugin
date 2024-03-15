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

let zigzag_encoding v =
  let open Infix.Int64 in
  let v = match v < 0L with
    | true -> v lsl 1 lxor (-1L)
    | false -> v lsl 1
  in
  v

let zigzag_encoding_unboxed v =
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
  | SInt64 -> write_varint ~f:zigzag_encoding
  | Int32 -> write_varint_unboxed ~f:Int32.to_int
  | UInt32 -> write_varint_unboxed ~f:Int32.to_int
  | SInt32 -> write_varint_unboxed ~f:(Int32.to_int @@ zigzag_encoding_unboxed)
  | Int64_int -> Writer.write_varint_unboxed_value
  | UInt64_int -> Writer.write_varint_unboxed_value
  | Int32_int -> Writer.write_varint_unboxed_value
  | UInt32_int -> Writer.write_varint_unboxed_value
  | SInt64_int -> write_varint_unboxed ~f:zigzag_encoding_unboxed
  | SInt32_int -> write_varint_unboxed ~f:zigzag_encoding_unboxed

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
    let write_key = write (Basic ((1, "key", "key"), key_spec, default_of_spec key_spec)) in
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
  let test vl =
    let v = Int64.to_int vl in
    Printf.printf "zigzag_encoding(%LdL) = %LdL\n" vl (zigzag_encoding vl);
    Printf.printf "zigzag_encoding_unboxed(%d) = %d\n" v (zigzag_encoding_unboxed v);
  in
  List.iter ~f:test [0L; -1L; 1L; -2L; 2L; 2147483647L; -2147483648L; Int64.max_int; Int64.min_int; ];
  [%expect {|
    zigzag_encoding(0L) = 0L
    zigzag_encoding_unboxed(0) = 0
    zigzag_encoding(-1L) = 1L
    zigzag_encoding_unboxed(-1) = 1
    zigzag_encoding(1L) = 2L
    zigzag_encoding_unboxed(1) = 2
    zigzag_encoding(-2L) = 3L
    zigzag_encoding_unboxed(-2) = 3
    zigzag_encoding(2L) = 4L
    zigzag_encoding_unboxed(2) = 4
    zigzag_encoding(2147483647L) = 4294967294L
    zigzag_encoding_unboxed(2147483647) = 4294967294
    zigzag_encoding(-2147483648L) = 4294967295L
    zigzag_encoding_unboxed(-2147483648) = 4294967295
    zigzag_encoding(9223372036854775807L) = -2L
    zigzag_encoding_unboxed(-1) = 1
    zigzag_encoding(-9223372036854775808L) = -1L
    zigzag_encoding_unboxed(0) = 0 |}]
