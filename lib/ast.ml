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

let rec pp = function
  | Lit_int  n  -> string_of_int n
  | Lit_bool b  -> string_of_bool b
  | Var      v  -> v
  | Binop (op, l, r)        -> Printf.sprintf "(%s %s %s)" (pp l) op (pp r)
  | Unop  (op, e)           -> Printf.sprintf "(%s %s)" op (pp e)
  | If    (c, t, e)         -> Printf.sprintf "(if %s then %s else %s)" (pp c) (pp t) (pp e)
  | Let   (x, d, b)         -> Printf.sprintf "(let %s = %s in %s)" x (pp d) (pp b)
  | Let_rec (f, x, d, b)    -> Printf.sprintf "(let rec %s %s = %s in %s)" f x (pp d) (pp b)
  | Fun   (p, b)            -> Printf.sprintf "(fun %s -> %s)" p (pp b)
  | App   (f, a)            -> Printf.sprintf "(%s %s)" (pp f) (pp a)
