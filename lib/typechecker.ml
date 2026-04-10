open Typ

(* ---- Fresh type variable supply ---- *)

let counter = ref 0

let fresh () =
  let n = !counter in
  incr counter;
  (* 'a .. 'z, then 't26, 't27, ... *)
  let name =
    if n < 26 then String.make 1 (Char.chr (n + Char.code 'a'))
    else "t" ^ string_of_int n
  in
  Var name

(* ---- Type environment ---- *)

(* Maps term-variable names to polymorphic type schemes. *)
type tenv = scheme SMap.t

let tenv_empty : tenv = SMap.empty

let free_vars_tenv env =
  SMap.fold (fun _ s acc -> SSet.union acc (free_vars_scheme_set s))
    env SSet.empty

let apply_subst_env s env = SMap.map (apply_subst_scheme s) env

(* Generalise: ∀-quantify all type vars free in [t] but not in [env]. *)
let generalise env t =
  let to_bind = SSet.diff (free_vars_set t) (free_vars_tenv env) in
  Forall (SSet.elements to_bind, t)

(* Instantiate a scheme with fresh type variables. *)
let instantiate (Forall (bound, t)) =
  let s = List.fold_left (fun acc v -> SMap.add v (fresh ()) acc)
            SMap.empty bound in
  apply_subst s t

(* ---- Unification (Robinson's algorithm) ---- *)

let rec unify t1 t2 =
  match t1, t2 with
  | Int,  Int  | Bool, Bool -> SMap.empty
  | Fun (a1, b1), Fun (a2, b2) ->
    let s1 = unify a1 a2 in
    let s2 = unify (apply_subst s1 b1) (apply_subst s1 b2) in
    compose s2 s1
  | Var v, t | t, Var v ->
    if t = Var v then SMap.empty                          (* same var, trivial *)
    else if SSet.mem v (free_vars_set t) then             (* occurs check *)
      failwith (Printf.sprintf
        "Type error: recursive type '%s occurs in %s" v (pp t))
    else SMap.singleton v t
  | _ ->
    failwith (Printf.sprintf
      "Type error: cannot unify %s with %s" (pp t1) (pp t2))

(* ---- Algorithm W ---- *)

(* Returns (substitution, inferred type).
   The returned type may still contain unresolved variables —
   apply the substitution to fully resolve it. *)
let rec infer_w (env : tenv) expr =
  match expr with
  | Ast.Lit_int  _ -> (SMap.empty, Int)
  | Ast.Lit_bool _ -> (SMap.empty, Bool)

  | Ast.Var name ->
    (match SMap.find_opt name env with
     | Some scheme -> (SMap.empty, instantiate scheme)
     | None -> failwith ("Unbound variable: " ^ name))

  (* fun x -> body  :  ty → typeof(body)[x:ty] *)
  | Ast.Fun (param, body) ->
    let ty = fresh () in
    let env' = SMap.add param (Forall ([], ty)) env in
    let (s, ty_body) = infer_w env' body in
    (s, Fun (apply_subst s ty, ty_body))

  (* e1 e2  :  infer e1 : ty → ret, infer e2 : ty, return ret *)
  | Ast.App (func, arg) ->
    let ret = fresh () in
    let (s1, ty_func) = infer_w env func in
    let (s2, ty_arg)  = infer_w (apply_subst_env s1 env) arg in
    let s3 = unify (apply_subst s2 ty_func) (Fun (ty_arg, ret)) in
    (compose s3 (compose s2 s1), apply_subst s3 ret)

  (* op e1 e2  :  check both args against operator's expected type *)
  | Ast.Binop (op, l, r) ->
    let (ty_in, ty_out) = binop_sig op in
    let (s1, tyl) = infer_w env l in
    let s2 = unify (apply_subst s1 tyl) ty_in in
    let s12 = compose s2 s1 in
    let (s3, tyr) = infer_w (apply_subst_env s12 env) r in
    let s4 = unify (apply_subst s3 tyr) ty_in in
    (compose s4 (compose s3 s12), ty_out)

  | Ast.Unop ("not", e) ->
    let (s1, ty) = infer_w env e in
    let s2 = unify ty Bool in
    (compose s2 s1, Bool)
  | Ast.Unop (op, _) -> failwith ("Unknown unary operator: " ^ op)

  (* if c then t else e  :  c:bool, t and e must agree *)
  | Ast.If (cond, then_, else_) ->
    let (s1, ty_c) = infer_w env cond in
    let s2 = unify ty_c Bool in
    let s12 = compose s2 s1 in
    let env' = apply_subst_env s12 env in
    let (s3, ty_t) = infer_w env' then_ in
    let (s4, ty_e) = infer_w (apply_subst_env s3 env') else_ in
    let s5 = unify (apply_subst s4 ty_t) ty_e in
    (compose s5 (compose s4 (compose s3 s12)), apply_subst s5 ty_e)

  (* let x = e1 in e2  :  generalise type of e1 for let-polymorphism *)
  | Ast.Let (name, def, body) ->
    let (s1, ty_def) = infer_w env def in
    let env' = apply_subst_env s1 env in
    let sc = generalise env' ty_def in
    let (s2, ty_body) = infer_w (SMap.add name sc env') body in
    (compose s2 s1, ty_body)

  (* let rec f x = def in body *)
  | Ast.Let_rec (fname, param, def, body) ->
    (* Give f and x fresh types, then infer the definition. *)
    let ty_f = fresh () and ty_x = fresh () in
    let env' =
      SMap.add fname (Forall ([], ty_f))
        (SMap.add param (Forall ([], ty_x)) env)
    in
    let (s1, ty_def) = infer_w env' def in
    (* f must have type ty_x → ty_def *)
    let s2 = unify (apply_subst s1 ty_f)
                   (Fun (apply_subst s1 ty_x, ty_def)) in
    let s = compose s2 s1 in
    (* Generalise f's fully-resolved type for the continuation. *)
    let ty_f2 = apply_subst s ty_f in
    let env'' = apply_subst_env s env in
    let sc_f = generalise env'' ty_f2 in
    let (s3, ty_body) = infer_w (SMap.add fname sc_f env'') body in
    (compose s3 s, ty_body)

(* Expected (input, output) types for each binary operator. *)
and binop_sig = function
  | "+" | "-" | "*" | "/" -> (Int,  Int)
  | "="  | "<" | ">"      -> (Int,  Bool)
  | "&&" | "||"           -> (Bool, Bool)
  | op -> failwith ("Unknown operator: " ^ op)

(* ---- Public API ---- *)

let infer expr =
  counter := 0;
  let (s, ty) = infer_w tenv_empty expr in
  apply_subst s ty

let check expr = ignore (infer expr)
