(** Hindley-Milner type inference via Algorithm W.

    Features:
    - Let-polymorphism: [let x = e1 in e2] generalises the type of [e1]
      before checking [e2], so [x] may be used at multiple types.
    - Occurs check: rejects recursive types like ['a = 'a -> 'b].
    - Principal types: always infers the most general type possible.
*)

val infer : Ast.expr -> Typ.t
(** [infer expr] infers the principal type of [expr] in the empty
    type environment. Raises [Failure] on type or unbound-variable errors. *)

val check : Ast.expr -> unit
(** [check expr] type-checks [expr], raising [Failure] on errors. *)
