type t = { enum_names:bool; json_names:bool; omit_default_values:bool }

(**
   Create options for json serialization.

   If [enum_names] is true then enums are serialized as strings. If false the integer value is used when serializing.

   If [json_name] is true then serialization will use the json field names. If false, the fields names will be used from the protofile as-is.

   If [omit_default_values] is false then default scalar values will not be emitted to the json. The default is to omit default values.
*)
let make ?(enum_names=true) ?(json_names=true) ?(omit_default_values=true) () =
  { enum_names; json_names; omit_default_values }
let default = make ()
