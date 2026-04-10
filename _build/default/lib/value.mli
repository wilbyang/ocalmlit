(** Runtime values produced by evaluation. *)

type t =
  | Int         of int
  | Bool        of bool
  | Closure     of string * Ast.expr * env
  | Rec_closure of string * string * Ast.expr * env

and env = (string * t) list

val pp : t -> string
(** Pretty-print a value as a human-readable string. *)
