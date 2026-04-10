(** Tree-walking evaluator for the mini functional language. *)

val eval : Env.t -> Ast.expr -> Value.t
(** [eval env expr] evaluates [expr] in environment [env],
    returning the resulting runtime value. Raises [Failure] on
    type errors, unbound variables, or division by zero. *)
