(** Extensible tree-walking evaluator built as an OCaml functor.

    Two hooks intercept evaluation:

    [wrap expr thunk]        — called for every expression node
    [on_apply func arg thunk]— called for every function application

    Together they enable tracing, step counting, memoisation, and
    any other strategy without duplicating the core logic.
*)

module type HOOKS = sig
  val wrap     : Ast.expr -> (unit -> Value.t) -> Value.t
  (** Intercept evaluation of a single expression node. *)

  val on_apply : Value.t -> Value.t -> (unit -> Value.t) -> Value.t
  (** Intercept a function application: [on_apply func arg thunk]
      is called just before [func] is applied to [arg].
      Calling [thunk ()] performs the actual application. *)
end

module Make (H : HOOKS) : sig
  val eval : Env.t -> Ast.expr -> Value.t
end

module No_hooks : HOOKS
(** Passes all calls straight through: [wrap _ f = f ()],
    [on_apply _ _ f = f ()]. *)

val eval : Env.t -> Ast.expr -> Value.t
(** Alias for [Make(No_hooks).eval]. *)
