(** Evaluation environment: maps variable names to runtime values.

    The representation is intentionally abstract.  Call sites never
    see whether [t] is a list, a balanced tree, or a hash table.
    This module is the single place that knows.
*)

type t
(** Abstract environment — currently a [Map.Make(String)] for O(log n)
    lookup and O(log n) extension. *)

val empty  : t
val lookup : string -> t -> Value.t
val extend : string -> Value.t -> t -> t

val of_list : Value.env -> t
(** Convert an association list (as stored inside closures) into an
    environment.  Later bindings for the same key shadow earlier ones,
    matching the behaviour of OCaml's own scoping rules. *)

val to_list : t -> Value.env
(** Snapshot the environment as an association list, for embedding
    inside a [Value.Closure] or [Value.Rec_closure]. *)
