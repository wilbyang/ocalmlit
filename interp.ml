(* ---------------------------------------------------------------
   Mini functional language interpreter — Step 1: naive baseline

   Language supports:
     - Integer and boolean literals
     - Arithmetic: +, -, *, /
     - Comparison: =, <, >
     - Boolean logic: &&, ||, not
     - If/else expressions
     - Let bindings: let x = e1 in e2
     - Lambdas: fun x -> e
     - Function application: f e
     - Recursive functions: let rec f x = e in body
   --------------------------------------------------------------- *)

(* ---- AST ---- *)

type expr =
  | Lit_int  of int
  | Lit_bool of bool
  | Var      of string
  | Binop    of string * expr * expr
  | Unop     of string * expr
  | If       of expr * expr * expr
  | Let      of string * expr * expr
  | Let_rec  of string * string * expr * expr   (* let rec f x = body in cont *)
  | Fun      of string * expr
  | App      of expr * expr

(* ---- Values ---- *)

type value =
  | Int    of int
  | Bool   of bool
  | Closure of string * expr * env        (* fun x -> e, captured env *)
  | Rec_closure of string * string * expr * env  (* f, x, body, env *)

and env = (string * value) list   (* association list: name -> value *)

(* ---- Environment helpers ---- *)

let env_lookup name env =
  match List.assoc_opt name env with
  | Some v -> v
  | None   -> failwith ("Unbound variable: " ^ name)

let env_extend name value env = (name, value) :: env

(* ---- Evaluator ---- *)

let rec eval env expr =
  match expr with
  | Lit_int  n -> Int n
  | Lit_bool b -> Bool b

  | Var name -> env_lookup name env

  | Binop (op, l, r) ->
    let lv = eval env l and rv = eval env r in
    eval_binop op lv rv

  | Unop ("not", e) ->
    (match eval env e with
     | Bool b -> Bool (not b)
     | _ -> failwith "not: expected bool")
  | Unop (op, _) -> failwith ("Unknown unary op: " ^ op)

  | If (cond, then_, else_) ->
    (match eval env cond with
     | Bool true  -> eval env then_
     | Bool false -> eval env else_
     | _ -> failwith "if: condition must be bool")

  | Let (name, def, body) ->
    let v = eval env def in
    eval (env_extend name v env) body

  | Let_rec (f, x, def, body) ->
    let closure = Rec_closure (f, x, def, env) in
    eval (env_extend f closure env) body

  | Fun (param, body) ->
    Closure (param, body, env)

  | App (func, arg) ->
    let fv = eval env func and av = eval env arg in
    apply fv av

and apply func arg =
  match func with
  | Closure (param, body, env) ->
    eval (env_extend param arg env) body
  | Rec_closure (f, x, body, env) ->
    let env' = env_extend x arg (env_extend f func env) in
    eval env' body
  | _ -> failwith "apply: not a function"

and eval_binop op lv rv =
  match op, lv, rv with
  | "+",  Int a, Int b  -> Int (a + b)
  | "-",  Int a, Int b  -> Int (a - b)
  | "*",  Int a, Int b  -> Int (a * b)
  | "/",  Int a, Int b  -> if b = 0 then failwith "Division by zero"
                           else Int (a / b)
  | "=",  Int a, Int b  -> Bool (a = b)
  | "<",  Int a, Int b  -> Bool (a < b)
  | ">",  Int a, Int b  -> Bool (a > b)
  | "&&", Bool a, Bool b -> Bool (a && b)
  | "||", Bool a, Bool b -> Bool (a || b)
  | _ -> failwith (Printf.sprintf "Type error in operator '%s'" op)

(* ---- Pretty printer ---- *)

let rec pp_value = function
  | Int n    -> string_of_int n
  | Bool b   -> string_of_bool b
  | Closure _     -> "<fun>"
  | Rec_closure _ -> "<rec-fun>"

(* ---- Tests ---- *)

let () =
  let run label expr =
    let result = eval [] expr in
    Printf.printf "%s => %s\n" label (pp_value result)
  in

  (* 1 + 2 * 3 = 7 *)
  run "1 + 2 * 3"
    (Binop ("+", Lit_int 1, Binop ("*", Lit_int 2, Lit_int 3)));

  (* let x = 10 in x + 5 = 15 *)
  run "let x = 10 in x + 5"
    (Let ("x", Lit_int 10, Binop ("+", Var "x", Lit_int 5)));

  (* if true then 42 else 0 = 42 *)
  run "if true then 42 else 0"
    (If (Lit_bool true, Lit_int 42, Lit_int 0));

  (* (fun x -> x * x) 7 = 49 *)
  run "(fun x -> x * x) 7"
    (App (Fun ("x", Binop ("*", Var "x", Var "x")), Lit_int 7));

  (* let add = fun x -> fun y -> x + y in add 3 4 = 7 *)
  run "curried add 3 4"
    (Let ("add",
      Fun ("x", Fun ("y", Binop ("+", Var "x", Var "y"))),
      App (App (Var "add", Lit_int 3), Lit_int 4)));

  (* let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 6 = 720 *)
  run "fact 6"
    (Let_rec ("fact", "n",
      If (Binop ("=", Var "n", Lit_int 0),
          Lit_int 1,
          Binop ("*", Var "n",
            App (Var "fact", Binop ("-", Var "n", Lit_int 1)))),
      App (Var "fact", Lit_int 6)));

  (* closure captures environment: let x = 3 in (fun y -> x + y) 4 = 7 *)
  run "closure capture"
    (Let ("x", Lit_int 3,
      App (Fun ("y", Binop ("+", Var "x", Var "y")), Lit_int 4)))
