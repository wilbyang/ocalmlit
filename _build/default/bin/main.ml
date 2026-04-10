open Ocamlit
open Ast

let run label expr =
  match Eval.eval Env.empty expr with
  | v -> Printf.printf "%-30s => %s\n" label (Value.pp v)
  | exception Failure msg -> Printf.printf "%-30s => ERROR: %s\n" label msg

let () =
  run "1 + 2 * 3"
    (Binop ("+", Lit_int 1, Binop ("*", Lit_int 2, Lit_int 3)));

  run "let x = 10 in x + 5"
    (Let ("x", Lit_int 10, Binop ("+", Var "x", Lit_int 5)));

  run "if true then 42 else 0"
    (If (Lit_bool true, Lit_int 42, Lit_int 0));

  run "(fun x -> x * x) 7"
    (App (Fun ("x", Binop ("*", Var "x", Var "x")), Lit_int 7));

  run "curried add 3 4"
    (Let ("add",
      Fun ("x", Fun ("y", Binop ("+", Var "x", Var "y"))),
      App (App (Var "add", Lit_int 3), Lit_int 4)));

  run "fact 6"
    (Let_rec ("fact", "n",
      If (Binop ("=", Var "n", Lit_int 0),
          Lit_int 1,
          Binop ("*", Var "n",
            App (Var "fact", Binop ("-", Var "n", Lit_int 1)))),
      App (Var "fact", Lit_int 6)));

  run "closure capture"
    (Let ("x", Lit_int 3,
      App (Fun ("y", Binop ("+", Var "x", Var "y")), Lit_int 4)));

  run "not false"
    (Unop ("not", Lit_bool false));

  run "10 / 2"
    (Binop ("/", Lit_int 10, Lit_int 2))
