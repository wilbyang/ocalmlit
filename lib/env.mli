(** Evaluation environment: maps variable names to runtime values. *)

type t = Value.env

val empty  : t
val lookup : string -> t -> Value.t
val extend : string -> Value.t -> t -> t
