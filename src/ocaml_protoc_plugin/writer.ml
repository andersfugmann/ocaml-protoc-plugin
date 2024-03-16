(** Some buffer to hold data, and to read and write data *)
open StdLabels
open Field

let length_delimited_size_field_length = 5

type substring = { mutable offset: int; buffer: Bytes.t }

type mode = Balanced | Speed | Space
type t = { mutable data: substring list; mode: mode; block_size:int }

let init ?(mode = Space) ?(block_size = 120) () =
  { data = []; mode; block_size }

let size t =
  let rec inner acc = function
    | [] -> acc
    | { offset; _} :: tl -> inner (offset + acc) tl
  in
  inner 0 t.data

let unused_space t =
  let rec inner acc = function
    | { offset; buffer } :: xs -> inner ((Bytes.length buffer) - offset + acc) xs
    | [] -> acc
  in
  inner 0 t.data

let write_varint buffer ~offset v =
  let rec inner ~offset v =
    let next_offset = offset + 1 in
    let open Infix.Int64 in
    match v lsr 7 with
    | 0L ->
      Bytes.unsafe_set buffer offset (Int64.to_int v |> Char.unsafe_chr);
      next_offset
    | rem ->
      Bytes.unsafe_set buffer offset ((v land 0x7fL) lor 0b1000_0000L |> Int64.to_int |> Char.unsafe_chr);
      inner ~offset:next_offset rem
  in
  inner ~offset v[@@unrolled 10]

let write_varint_unboxed buffer ~offset v = write_varint buffer ~offset (Int64.of_int v)

(** Write a field delimited length.
    A delimited field length can be no larger than 2^31.
    This function always write 5 bytes (7*5bits = 35bits > 31bits).
    This allows the field length to be statically allocated and written later.
    The spec does not forbid this encoding, but there might be implementation
    that disallow '0' as the ending varint value.
*)
let write_delimited_field_length_fixed_size buffer ~offset v =
  (* Set the 34'th bit to make sure all bytes are written. Then clear it again *)
  let vl = Int64.of_int v in
  let offset = write_varint buffer ~offset Int64.(logor vl 0x400000000L) in
  let v = Bytes.get_uint8 buffer (offset - 1) in
  Bytes.set_uint8 buffer (offset-1) (v land 0b0011_1111);
  offset


let ensure_capacity ~size t =
  match t.data with
  | { offset; buffer } as elem :: _ when Bytes.length buffer - offset >= size -> elem
  | tl ->
    let elem = { offset = 0; buffer = Bytes.create (max size t.block_size) } in
    t.data <- elem :: tl;
    elem

(** Direct functions *)
let write_const_value data t =
  let len = String.length data in
  let elem = ensure_capacity ~size:len t in
  Bytes.blit_string ~src:data ~src_pos:0 ~dst:elem.buffer ~dst_pos:elem.offset ~len;
  elem.offset <- elem.offset + len

let write_fixed32_value: int32 -> t -> unit = fun v t ->
  let elem = ensure_capacity ~size:4 t in
  Bytes.set_int32_le elem.buffer elem.offset v;
  elem.offset <- elem.offset + 4

let write_fixed64_value: int64 -> t -> unit = fun v t ->
  let elem = ensure_capacity ~size:8 t in
  Bytes.set_int64_le elem.buffer elem.offset v;
  elem.offset <- elem.offset + 8

let write_varint_unboxed_value: int -> t -> unit = fun v t ->
  let elem = ensure_capacity ~size:10 t in
  let offset = write_varint_unboxed elem.buffer ~offset:elem.offset v in
  elem.offset <- offset

let write_varint_value: int64 -> t -> unit = fun v t ->
  let elem = ensure_capacity ~size:10 t in
  let offset = write_varint elem.buffer ~offset:elem.offset v in
  elem.offset <- offset

let write_length_delimited_value: data:string -> offset:int -> len:int -> t -> unit = fun ~data ~offset ~len t ->
  write_varint_unboxed_value len t;
  let elem = ensure_capacity ~size:len t in
  Bytes.blit_string ~src:data ~src_pos:offset ~dst:elem.buffer ~dst_pos:elem.offset ~len;
  elem.offset <- elem.offset + len

let write_field_header : t -> int -> int -> unit = fun t index field_type ->
  let header = (index lsl 3) + field_type in
  write_varint_unboxed_value header t

let write_field : t -> int -> Field.t -> unit = fun t index field ->
  let field_type, writer =
    match field with
    | Varint v ->
      0, write_varint_value v
    | Varint_unboxed v ->
      0, write_varint_unboxed_value v
    | Fixed_64_bit v ->
      1, write_fixed64_value v
    | Length_delimited {offset; length; data} ->
      2, write_length_delimited_value ~data ~offset ~len:length
    | Fixed_32_bit v ->
      5, write_fixed32_value v
  in
  write_field_header t index field_type;
  writer t


let write_length_delimited_f ~write_f v t =
  let rec size_data_added sentinel acc = function
    | [] -> failwith "End of list reached. This is impossible"
    | x :: _ when x == sentinel -> acc (* Physical equality intended *)
    | { offset; _ } :: xs -> size_data_added sentinel (offset + acc) xs
  in
  let write_balanced v t =
    let sentinel =
      match t.data with
      | { offset; buffer} as sentinel :: _ when offset + length_delimited_size_field_length <= Bytes.length buffer ->
        sentinel
      | _ ->
        let sentinel = { offset = 0; buffer = Bytes.create length_delimited_size_field_length; } in
        t.data <- sentinel :: t.data;
        sentinel
    in
    let offset = sentinel.offset in
    (* Ensure no writes to the sentinel *)
    sentinel.offset <- Int.max_int;
    let () = write_f t v in
    let size = size_data_added sentinel 0 t.data in
    let offset = write_varint_unboxed sentinel.buffer ~offset size in
    sentinel.offset <- offset;
    ()
  in
  let write_speed v t =
    let sentinel = ensure_capacity ~size:length_delimited_size_field_length t in
    let offset = sentinel.offset in
    sentinel.offset <- sentinel.offset + length_delimited_size_field_length;
    let () = write_f t v in
    let size = size_data_added sentinel (sentinel.offset - (offset + length_delimited_size_field_length)) t.data in
    let _ = write_delimited_field_length_fixed_size sentinel.buffer ~offset size in
    ()
  in
  let write_space v t =
    let sentinel = ensure_capacity ~size:length_delimited_size_field_length t in
    let offset = sentinel.offset in
    sentinel.offset <- sentinel.offset + length_delimited_size_field_length;
    let () = write_f t v in
    let size = size_data_added sentinel (sentinel.offset - (offset + length_delimited_size_field_length)) t.data in
    let offset' = write_varint_unboxed sentinel.buffer ~offset size in
    (* Move data to avoid holes *)
    let () = match (offset + length_delimited_size_field_length = offset') with
      | true -> ()
      | false ->
        Bytes.blit ~src:sentinel.buffer ~src_pos:(offset + length_delimited_size_field_length)
          ~dst:sentinel.buffer ~dst_pos:offset'
          ~len:(sentinel.offset - (offset + length_delimited_size_field_length));
        sentinel.offset <- sentinel.offset - (offset+length_delimited_size_field_length-offset');
    in
    ()
  in
  match t.mode with
  | Balanced -> write_balanced v t
  | Speed -> write_speed v t
  | Space -> write_space v t

let contents t =
  let size = size t in
  let contents = Bytes.create size in
  let rec inner offset = function
    | [] -> offset
    | { offset = o; buffer} :: tl ->
       let next_offset = offset - o in
       Bytes.blit ~src:buffer ~src_pos:0 ~dst:contents ~dst_pos:next_offset ~len:o;
       inner (next_offset) tl
  in
  let offset = inner size t.data in
  assert (offset = 0);
  Bytes.unsafe_to_string contents

let dump t =
  let string_contents = contents t in
  List.init ~len:(String.length string_contents) ~f:(fun i ->
    Printf.sprintf "%02x" (Char.code (String.get string_contents i))
  )
  |> String.concat ~sep:"-"
  |> Printf.printf "Buffer: %s\n"

let string_of_bytes b =
  Bytes.to_seq b |> Seq.map Char.code |> Seq.map (Printf.sprintf "%02x") |> List.of_seq |> String.concat ~sep:" "

let of_list: (int * Field.t) list -> t = fun fields ->
  let t = init () in
  List.iter ~f:(fun (index, field) -> write_field t index field) fields;
  t

let%expect_test "Writefield" =
  let buffer = init () in
  write_field buffer 1 (Varint 3L);
  write_field buffer 2 (Varint 5L);
  write_field buffer 3 (Varint 7L);
  write_field buffer 4 (Varint 11L);
  dump buffer;
  [%expect {| Buffer: 08-03-10-05-18-07-20-0b |}]

let%expect_test "fixed_size" =
  List.iter ~f:(fun v ->
    let buffer = Bytes.make 10 '\255' in
    let _ = write_delimited_field_length_fixed_size buffer ~offset:0 v in
    Printf.printf "Fixed field: 0x%08x: %s\n" v (string_of_bytes buffer);
  ) [0;1;2;0x7fffffff; 0x3fffffff];
  ();
  [%expect {|
    Fixed field: 0x00000000: 80 80 80 80 00 ff ff ff ff ff
    Fixed field: 0x00000001: 81 80 80 80 00 ff ff ff ff ff
    Fixed field: 0x00000002: 82 80 80 80 00 ff ff ff ff ff
    Fixed field: 0x7fffffff: ff ff ff ff 07 ff ff ff ff ff
    Fixed field: 0x3fffffff: ff ff ff ff 03 ff ff ff ff ff |}]


let%test "varint" =
  let open Infix.Int64 in
  let string_of_bytes b =
    Bytes.to_seq b |> Seq.map Char.code |> Seq.map (Printf.sprintf "%02x") |> List.of_seq |> String.concat ~sep:" "
  in
  let values = List.init ~len:64 ~f:(fun idx -> 1L lsl idx) @
               List.init ~len:64 ~f:(fun idx -> (-1L) lsl idx)
  in
  List.fold_left ~init:true ~f:(fun acc v ->
    List.fold_left ~init:acc ~f:(fun acc v ->
      let b1 = Bytes.make 10 '\000' in
      let b2 = Bytes.make 10 '\000' in
      write_varint_unboxed b1 ~offset:0 (Int64.to_int v) |> ignore;
      write_varint b2 ~offset:0 (v) |> ignore;
      match Bytes.equal b1 b2 || Int64.shift_right_logical v 63 != 0L with
      | true -> acc
      | false ->
        Printf.printf "Unboxed: %16Lx (%20d): %S = %S\n" v (Int64.to_int v) (string_of_bytes b1) (string_of_bytes b2);
        false
    ) [v-2L; v-1L; v; v+1L; v+2L]
  ) values
