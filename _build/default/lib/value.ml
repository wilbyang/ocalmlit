type t =
  | Int         of int
  | Bool        of bool
  | Closure     of string * Ast.expr * env
  | Rec_closure of string * string * Ast.expr * env

and env = (string * t) list

let pp = function
  | Int n         -> string_of_int n
  | Bool b        -> string_of_bool b
  | Closure _     -> "<fun>"
  | Rec_closure _ -> "<rec-fun>"
