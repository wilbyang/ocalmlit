open Ocamlit

(* ================================================================
   Strategy 1 — Standard (Make(No_hooks), aliased as Eval.eval)
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
   ================================================================ *)

module Step_hooks = struct
  let count = ref 0
  let reset () = count := 0
  let wrap _ f     = incr count; f ()
  let on_apply _ _ f = f ()          (* applications counted via wrap on body *)
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
   Strategy 3 — Tracer
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

  let on_apply _ _ f = f ()
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
   Strategy 4 — Memoising evaluator
   Caches calls to named recursive functions on integer arguments.
   Turns fib from O(2^n) to O(n) evaluations.
   ================================================================ *)

module Memo_hooks = struct
  (* Key: (recursive_function_name, int_argument) *)
  let cache : (string * int, Value.t) Hashtbl.t = Hashtbl.create 64
  let reset ()  = Hashtbl.clear cache

  let wrap _ f = f ()

  let on_apply func arg f =
    match func, arg with
    | Value.Rec_closure (name, _, _, _), Value.Int n ->
      let key = (name, n) in
      (match Hashtbl.find_opt cache key with
       | Some v -> v                          (* cache hit — skip recursion *)
       | None   ->
         let v = f () in                      (* compute, then cache *)
         Hashtbl.replace cache key v;
         v)
    | _ -> f ()                              (* non-recursive or non-int: pass through *)
end

module Memo_eval = Eval.Make (Memo_hooks)

let run_memo label src =
  match Parser.parse src with
  | exception Failure msg -> Printf.printf "%s  PARSE ERROR: %s\n" label msg
  | expr ->
    (* also count steps to compare with the naive version *)
    Step_hooks.reset ();
    Memo_hooks.reset ();
    (* build a combined counting+memoising evaluator inline *)
    let module Combined = Eval.Make (struct
      let wrap expr f  = Step_hooks.wrap expr f
      let on_apply func arg f = Memo_hooks.on_apply func arg f
    end) in
    let v = Combined.eval Env.empty expr in
    Printf.printf "%-36s  =>  %-6s  (%d steps, memoised)\n"
      label (Value.pp v) !Step_hooks.count

(* ================================================================
   Main
   ================================================================ *)

let () =
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

  print_endline "=== Step counter: naive vs memoised ===";
  let fib_src = "let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib " in
  List.iter (fun n ->
    let src = fib_src ^ string_of_int n in
    let label = Printf.sprintf "fib %d" n in
    run_counted label src;
    run_memo    label src;
    print_newline ()
  ) [10; 20; 30];

  print_endline "=== Tracer (small example) ===";
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
