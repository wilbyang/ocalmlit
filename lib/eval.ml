open Ast
open Value

module type HOOKS = sig
  val wrap     : Ast.expr -> (unit -> Value.t) -> Value.t
  val on_apply : Value.t -> Value.t -> (unit -> Value.t) -> Value.t
end

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
        (* Capture the current env as a list; the evaluator works with
           Env.t (the Map) but Value.t stores Value.env (the list). *)
        let closure = Rec_closure (f, x, def, Env.to_list env) in
        eval (Env.extend f closure env) body

      | Fun (param, body) ->
        (* Snapshot the live Map env into the closure's association list. *)
        Closure (param, body, Env.to_list env)

      | App (func, arg) ->
        apply (eval env func) (eval env arg)
    )

  and apply func arg =
    H.on_apply func arg (fun () ->
      match func with
      | Closure (param, body, captured) ->
        (* Reconstruct a Map env from the captured list, then extend. *)
        eval (Env.extend param arg (Env.of_list captured)) body
      | Rec_closure (f, x, body, captured) ->
        let env = Env.extend x arg
                    (Env.extend f func (Env.of_list captured)) in
        eval env body
      | _ -> failwith "apply: not a function"
    )

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

module No_hooks = struct
  let wrap _ f     = f ()
  let on_apply _ _ f = f ()
end

include Make (No_hooks)
