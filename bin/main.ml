open Ocamlit

(* ================================================================
   Strategy 1 — Standard (no hooks)
   Eval.eval is already Make(No_hooks).eval via [include].
   ================================================================ *)

let run label src =
  match Parser.parse src with
  | exception Failure msg -> Printf.printf "%-36s  PARSE ERROR: %s\n" label msg
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

(* ================================================================
   Strategy 2 — Step counter
   Counts every expression node visited during evaluation.
   ================================================================ *)

module Step_hooks = struct
  let count = ref 0
  let reset () = count := 0
  (* wrap: increment the counter, then evaluate normally *)
  let wrap _ f = incr count; f ()
end

module Counting_eval = Eval.Make (Step_hooks)

let run_counted label src =
  match Parser.parse src with
  | exception Failure msg -> Printf.printf "%s  PARSE ERROR: %s\n" label msg
  | expr ->
    Step_hooks.reset ();
    let v = Counting_eval.eval Env.empty expr in
    Printf.printf "%-36s  =>  %-6s  (%d steps)\n"
      label (Value.pp v) !Step_hooks.count

(* ================================================================
   Strategy 3 — Tracing evaluator
   Prints each expression as it is evaluated, indented by call depth.
   ================================================================ *)

module Trace_hooks = struct
  let depth = ref 0

  let wrap expr f =
    let pad = String.make (!depth * 2) ' ' in
    Printf.printf "%seval: %s\n" pad (Ast.pp expr);
    incr depth;
    let v = f () in
    decr depth;
    Printf.printf "%s   =  %s\n" pad (Value.pp v);
    v
end

module Tracing_eval = Eval.Make (Trace_hooks)

let run_traced src =
  match Parser.parse src with
  | exception Failure msg -> Printf.printf "PARSE ERROR: %s\n" msg
  | expr ->
    Trace_hooks.depth := 0;
    Printf.printf "Tracing: %s\n%s\n" src (String.make 60 '-');
    let v = Tracing_eval.eval Env.empty expr in
    Printf.printf "%s\nResult: %s\n\n" (String.make 60 '-') (Value.pp v)

(* ================================================================
   Main
   ================================================================ *)

let () =
  (* --- Standard tests --- *)
  print_endline "=== Standard evaluator ===";
  run "1 + 2 * 3"              "1 + 2 * 3";
  run "let x = 10 in x + 5"   "let x = 10 in x + 5";
  run "if true then 42 else 0" "if true then 42 else 0";
  run "(fun x -> x * x) 7"    "(fun x -> x * x) 7";
  run "curried add 3 4"
    "let add = fun x -> fun y -> x + y in add 3 4";
  run "fact 6"
    "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 6";
  run "fib 10"
    "let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib 10";
  run "polymorphic id"
    "let id = fun x -> x in if id true then id 1 else id 2";
  print_newline ();

  (* --- Step counter --- *)
  print_endline "=== Step counter (Make(Step_hooks)) ===";
  run_counted "1 + 2 * 3"
    "1 + 2 * 3";
  run_counted "fact 6"
    "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 6";
  run_counted "fib 10"
    "let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib 10";
  print_newline ();

  (* --- Tracer (small example to keep output readable) --- *)
  print_endline "=== Tracer (Make(Trace_hooks)) ===";
  run_traced "(fun x -> x + 1) 3"

(* ================================================================
   REPL
   ================================================================ *)

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--repl" then begin
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
  end
