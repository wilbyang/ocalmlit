module SMap = Map.Make (String)

(* Changed from [(string * Value.t) list] to a balanced BST map.
   Lookup is now O(log n); extend is O(log n); the rest of the
   codebase is unaffected because [t] is abstract in the interface. *)
type t = Value.t SMap.t

let empty = SMap.empty

let lookup name env =
  match SMap.find_opt name env with
  | Some v -> v
  | None   -> failwith ("Unbound variable: " ^ name)

(* SMap.add replaces any previous binding for [name], which gives us
   correct lexical shadowing: the innermost binding wins. *)
let extend name v env = SMap.add name v env

(* Closure capture: list → map.
   If the same name appears more than once in [lst] (from nested lets),
   the leftmost occurrence (the most recent binding) wins because we
   fold left and later SMap.add calls overwrite earlier ones. *)
let of_list lst =
  List.fold_left (fun m (k, v) -> SMap.add k v m) SMap.empty lst

(* Snapshot for embedding into a Value.Closure.
   The map has at most one entry per name, so the resulting list is
   always duplicate-free, unlike the old association-list environment. *)
let to_list env =
  SMap.fold (fun k v acc -> (k, v) :: acc) env []
