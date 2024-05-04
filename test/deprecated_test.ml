open Deprecated
[@@@ocaml.alert "-protobuf"] (* Disable deprecation warnings for protobuf*)
[@@@warning "-32"]

module T1 = Message1 (* Message deprecated *)
module T2 = Message2
type _t1 = Message1.t (* Message deprecated *)
type _t2 = Message2.t (* Field deprecated *)


module E1' = E1 (* Enum deprecated *)
let _ = E1.E1 (* Enum deprecated *)
let _ = E2.E2 (* Enum value deprecated *)
let _ = E2.E3

let _ : Message2.t = 4 (* Field deprecated *)

let _ = Message3.{ a = 4; (* Field Deprecated *)
                    b = 5;
                    c = `X 5}

let _ = Service1.Method1.name (* Service deprecated *)
let _ = Service2.Method1.name (* Method deprecated *)
let _ = Service2.Method2.name
let _ = Service1.method1 (* Service deprecated *)
