open Ocamlit

(* ---- Test suite ---- *)

let run label src =
  match Parser.parse src with
  | exception Failure msg ->
    Printf.printf "%-36s  PARSE ERROR: %s\n" label msg
  | expr ->
    let typ_str =
      match Typechecker.infer expr with
      | t -> Typ.pp t
      | exception Failure msg -> "TYPE ERROR: " ^ msg
    in
    let val_str =
      match Eval.eval Env.empty expr with
      | v -> Value.pp v
      | exception Failure msg -> "EVAL ERROR: " ^ msg
    in
    Printf.printf "%-36s  : %-20s  =>  %s\n" label typ_str val_str

let () =
  print_endline "=== Tests ===";
  run "1 + 2 * 3"              "1 + 2 * 3";
  run "let x = 10 in x + 5"   "let x = 10 in x + 5";
  run "if true then 42 else 0" "if true then 42 else 0";
  run "(fun x -> x * x) 7"    "(fun x -> x * x) 7";
  run "curried add 3 4"
    "let add = fun x -> fun y -> x + y in add 3 4";
  run "fact 6"
    "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 6";
  run "closure capture"
    "let x = 3 in (fun y -> x + y) 4";
  run "not false"              "not false";
  run "10 / 2"                 "10 / 2";
  run "fib 10"
    "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10";
  run "identity applied"
    "let id = fun x -> x in id 42";
  run "polymorphic id (bool)"
    "let id = fun x -> x in if id true then id 1 else id 2";
  run "higher-order"
    "let apply = fun f -> fun x -> f x in apply (fun n -> n * n) 9";
  print_newline ();
  (* Type error examples — should report errors, not crash *)
  print_endline "=== Type errors (expected) ===";
  run "if 1 then 2 else 3"     "if 1 then 2 else 3";
  run "1 + true"               "1 + true";
  run "true && 42"             "true && 42";
  print_newline ()

(* ---- REPL ---- *)

let repl () =
  print_endline "ocamlit REPL  (type 'exit' to quit)";
  print_endline "------------------------------------";
  let rec loop () =
    print_string "> ";
    flush stdout;
    match input_line stdin with
    | exception End_of_file -> print_newline ()
    | "exit" | "quit"       -> ()
    | ""                    -> loop ()
    | src ->
      (match Parser.parse src with
       | exception Failure msg -> Printf.printf "Parse error: %s\n" msg
       | expr ->
         (match Typechecker.infer expr with
          | exception Failure msg -> Printf.printf "Type error: %s\n" msg
          | typ ->
            match Eval.eval Env.empty expr with
            | v   -> Printf.printf "- : %s = %s\n" (Typ.pp typ) (Value.pp v)
            | exception Failure msg -> Printf.printf "Eval error: %s\n" msg));
      loop ()
  in
  loop ()

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--repl"
  then repl ()
