(* Consider changeing the Length_delimited to be a substring *)
(* Also consider changeing the writer to create a list of fields,
   and only convert these into a string at the very end.
   This would save data copying.
*)

type field =
  | Varint of int (* int32, int64, uint32, uint64, sint32, sint64, bool, enum *)
  | Fixed_64_bit of Int64.t (* fixed64, sfixed64, double *)
  | Length_delimited of { offset: int; length: int; data: string } (* string, bytes, embedded messages, packed repeated fields *)
  | Fixed_32_bit of Int32.t (* fixed32, sfixed32, float *)
[@@deriving show]

let varint v = Varint v
let fixed_32_bit v = Fixed_32_bit v
let fixed_64_bit v = Fixed_64_bit v
let length_delimited ~offset ~length data = Length_delimited { offset; length; data }
