type t =
  | Int
  | Bool
  | Fun of t * t
  | Var of string

type scheme = Forall of string list * t

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type subst = t SMap.t

let rec apply_subst s = function
  | Int        -> Int
  | Bool       -> Bool
  | Fun (a, b) -> Fun (apply_subst s a, apply_subst s b)
  | Var v      -> (match SMap.find_opt v s with Some t -> t | None -> Var v)

let apply_subst_scheme s (Forall (bound, t)) =
  (* Remove bound vars from the substitution so they aren't replaced. *)
  let s' = List.fold_left (fun acc v -> SMap.remove v acc) s bound in
  Forall (bound, apply_subst s' t)

let rec free_vars_set = function
  | Int | Bool -> SSet.empty
  | Fun (a, b) -> SSet.union (free_vars_set a) (free_vars_set b)
  | Var v      -> SSet.singleton v

let free_vars_scheme_set (Forall (bound, t)) =
  SSet.diff (free_vars_set t) (SSet.of_list bound)

(* (s1 ∘ s2)(x) = s1(s2(x)):
   apply s1 to every type in s2, then add s1's own bindings. *)
let compose s1 s2 =
  let s2' = SMap.map (apply_subst s1) s2 in
  SMap.union (fun _ v _ -> Some v) s1 s2'

let rec pp = function
  | Int        -> "int"
  | Bool       -> "bool"
  | Var v      -> "'" ^ v
  | Fun (a, b) ->
    (* -> is right-associative: only parenthesise the left arg if it's also Fun *)
    let lhs = match a with Fun _ -> "(" ^ pp a ^ ")" | _ -> pp a in
    lhs ^ " -> " ^ pp b
