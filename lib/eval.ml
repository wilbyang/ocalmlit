open Ast
open Value

(* ---- Hook signature ---- *)

module type HOOKS = sig
  val wrap : Ast.expr -> (unit -> Value.t) -> Value.t
end

(* ---- Core evaluator functor ---- *)

module Make (H : HOOKS) = struct

  let rec eval env expr =
    H.wrap expr (fun () ->
      match expr with
      | Lit_int  n -> Int n
      | Lit_bool b -> Bool b

      | Var name -> Env.lookup name env

      | Binop (op, l, r) ->
        eval_binop op (eval env l) (eval env r)

      | Unop ("not", e) ->
        (match eval env e with
         | Bool b -> Bool (not b)
         | _      -> failwith "not: expected bool")
      | Unop (op, _) -> failwith ("Unknown unary operator: " ^ op)

      | If (cond, then_, else_) ->
        (match eval env cond with
         | Bool true  -> eval env then_
         | Bool false -> eval env else_
         | _          -> failwith "if: condition must be bool")

      | Let (name, def, body) ->
        eval (Env.extend name (eval env def) env) body

      | Let_rec (f, x, def, body) ->
        eval (Env.extend f (Rec_closure (f, x, def, env)) env) body

      | Fun (param, body) ->
        Closure (param, body, env)

      | App (func, arg) ->
        apply (eval env func) (eval env arg)
    )

  and apply func arg =
    match func with
    | Closure (param, body, env) ->
      eval (Env.extend param arg env) body
    | Rec_closure (f, x, body, env) ->
      eval (Env.extend x arg (Env.extend f func env)) body
    | _ -> failwith "apply: not a function"

  and eval_binop op lv rv =
    match op, lv, rv with
    | "+",  Int a,  Int b  -> Int (a + b)
    | "-",  Int a,  Int b  -> Int (a - b)
    | "*",  Int a,  Int b  -> Int (a * b)
    | "/",  Int a,  Int b  ->
      if b = 0 then failwith "Division by zero" else Int (a / b)
    | "=",  Int a,  Int b  -> Bool (a = b)
    | "<",  Int a,  Int b  -> Bool (a < b)
    | ">",  Int a,  Int b  -> Bool (a > b)
    | "&&", Bool a, Bool b -> Bool (a && b)
    | "||", Bool a, Bool b -> Bool (a || b)
    | _ -> failwith (Printf.sprintf "Type error in '%s'" op)

end

(* ---- Built-in hook implementations ---- *)

module No_hooks = struct
  let wrap _ f = f ()
end

(* ---- Default evaluator (backward-compatible) ---- *)

include Make (No_hooks)
