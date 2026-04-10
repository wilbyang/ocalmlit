(** Abstract syntax tree for the mini functional language. *)

type expr =
  | Lit_int  of int
  | Lit_bool of bool
  | Var      of string
  | Binop    of string * expr * expr
  | Unop     of string * expr
  | If       of expr * expr * expr
  | Let      of string * expr * expr
  | Let_rec  of string * string * expr * expr
  | Fun      of string * expr
  | App      of expr * expr
