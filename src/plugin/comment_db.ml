open StdLabels
open MoreLabels
open Spec.Descriptor.Google.Protobuf

type element =
  | Message | Field
  | Enum | Enum_value
  | Oneof
  | Service | Method
  | Extension
  | File
  | Unknown

let element_of_int ~context = function
  | 4 when context = File -> Message
  | 3 when context = Message -> Message

  | 5 when context = File -> Enum
  | 4 when context = Message -> Enum
  | 2 when context = Enum -> Enum_value

  | 8 when context = Message -> Oneof
  | 2 when context = Message -> Field

  | 6 when context = File -> Service
  | 2 when context = Service -> Method

  | 7 when context = File -> Extension
  | 6 when context = Message -> Extension

  | _ -> Unknown

type path = (element * int) list
module Db = Map.Make(struct type t = path let compare = compare end)

type comment = string option
type document = { leading: comment; trailing: comment; detatched: string list }
type t = document Db.t

let make: SourceCodeInfo.t option -> t = fun source_code_info ->
  let source_code_info = Option.value ~default:[] source_code_info in
  (* It would be simpler if I could peel off each layer as I traverse *)
  (* At least the path should be reversed, as its easier to append *)
  (* And the path should be in pairs???? - That is not what the documentation says *)

  (* For each source code info field, parse the list. Need to remeber the current context *)
  (* Remember, we dont want comments for individual names *)

  let rec map_location ~context = function
    | field_id :: number :: rest ->
      let element = element_of_int ~context field_id in
      (element, number) :: map_location ~context:element rest
    | [ _ ] | [] -> []
  in

  List.fold_left ~init:Db.empty ~f:(fun db location ->
    let path = map_location ~context:File location.SourceCodeInfo.Location.path in
    let element =
      { leading = location.SourceCodeInfo.Location.leading_comments;
        trailing = location.SourceCodeInfo.Location.trailing_comments;
        detatched = location.SourceCodeInfo.Location.leading_detached_comments;
      }
    in
    Db.add ~key:path ~data:element db
  ) source_code_info
