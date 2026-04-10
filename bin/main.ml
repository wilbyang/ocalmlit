open Ocamlit

(* ---- Test suite (using the parser now) ---- *)

let run label src =
  match Parser.parse src |> Eval.eval Env.empty with
  | v -> Printf.printf "%-36s => %s\n" label (Value.pp v)
  | exception Failure msg -> Printf.printf "%-36s => ERROR: %s\n" label msg

let () =
  print_endline "=== Tests ===";
  run "1 + 2 * 3"             "1 + 2 * 3";
  run "let x = 10 in x + 5"  "let x = 10 in x + 5";
  run "if true then 42 else 0" "if true then 42 else 0";
  run "(fun x -> x * x) 7"   "(fun x -> x * x) 7";
  run "curried add 3 4"
    "let add = fun x -> fun y -> x + y in add 3 4";
  run "fact 6"
    "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 6";
  run "closure capture"
    "let x = 3 in (fun y -> x + y) 4";
  run "not false"             "not false";
  run "10 / 2"                "10 / 2";
  run "fib 10"
    "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10";
  run "higher-order: map-like"
    "let apply = fun f -> fun x -> f x in apply (fun n -> n * n) 9";
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
      (match Parser.parse src |> Eval.eval Env.empty with
       | v -> print_endline (Value.pp v)
       | exception Failure msg -> Printf.printf "Error: %s\n" msg);
      loop ()
  in
  loop ()

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--repl"
  then repl ()
