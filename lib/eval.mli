(** Extensible tree-walking evaluator built as an OCaml functor.

    The evaluation strategy is abstracted behind a [HOOKS] module.
    [HOOKS.wrap expr thunk] is called for every expression node:
    it receives the expression and a thunk that performs the actual
    evaluation, and may do anything before, after, or instead of it.

    This single hook covers:
    - tracing  (print before/after each step)
    - counting  (increment a counter per step)
    - memoising (cache results by expression)
    - short-circuiting (skip evaluation of known subterms)

    The core evaluation logic lives entirely in [Make] and is never
    duplicated across strategies.
*)

module type HOOKS = sig
  val wrap : Ast.expr -> (unit -> Value.t) -> Value.t
  (** [wrap expr thunk] intercepts the evaluation of [expr].
      Calling [thunk ()] performs the actual evaluation step. *)
end

module Make (H : HOOKS) : sig
  val eval : Env.t -> Ast.expr -> Value.t
end
(** Produce a complete evaluator from any [HOOKS] implementation. *)

module No_hooks : HOOKS
(** Identity hooks — [wrap _ f = f ()].  The baseline strategy. *)

val eval : Env.t -> Ast.expr -> Value.t
(** Convenience alias for [Make(No_hooks).eval]. *)
