module Json = Json
module Reader = Reader
module Writer = Writer
module Service = Service
module Result = Result
module Extensions = Extensions
module Json_options = Json_options

(**/**)
module Serialize = Serialize
module Deserialize = Deserialize
module Serialize_json = Serialize_json
module Deserialize_json = Deserialize_json
module Spec = Spec
module Field = Field
module Merge = Merge

let[@inline] apply_lazy f =
  match Sys.backend_type with
  | Native | Bytecode ->
    f ()
  | Other _ ->
    let f = Lazy.from_fun f in
    fun x -> (Lazy.force f) x
(**/**)

type test =
  { a: int; (** This is an int *)
    b: [`First | `Second]; (** {e b} is [b]
                               - `First is ok
                               - `Second is also
                           *)
  }
