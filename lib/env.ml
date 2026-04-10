type t = Value.env

let empty = []

let lookup name env =
  match List.assoc_opt name env with
  | Some v -> v
  | None   -> failwith ("Unbound variable: " ^ name)

let extend name value env = (name, value) :: env
