(** Type representations, substitutions, and type schemes. *)

(** A monomorphic type term. *)
type t =
  | Int
  | Bool
  | Fun of t * t
  | Var of string       (** unification variable, e.g. 'a *)

(** A polymorphic type scheme: ∀ bound_vars. body *)
type scheme = Forall of string list * t

module SMap : Map.S with type key = string
module SSet : Set.S with type elt = string

(** A substitution maps type-variable names to types. *)
type subst = t SMap.t

val apply_subst        : subst -> t      -> t
val apply_subst_scheme : subst -> scheme -> scheme

val free_vars_set        : t      -> SSet.t
val free_vars_scheme_set : scheme -> SSet.t

(** [compose s1 s2] is the substitution (s1 ∘ s2), i.e. apply s2 first. *)
val compose : subst -> subst -> subst

val pp : t -> string
(** Pretty-print a type, e.g. [int -> 'a -> bool]. *)
