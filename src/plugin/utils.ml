open StdLabels
open MoreLabels

let failwith_f fmt =
  Printf.ksprintf (fun s -> failwith s) fmt

module StringMap = struct
  include Map.Make(String)

  (** Fail with an error if the key already exists *)
  let add_uniq ~key ~data map =
    update ~key ~f:(function
      | None -> Some data
      | Some _ -> failwith_f "Key %s already exists" key
    ) map
end
module StringSet = Set.Make(String)
