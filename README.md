# ocamlit

A mini functional language interpreter built progressively in OCaml,
used as a vehicle for learning idiomatic OCaml — modules, functors,
the type system, and clean abstraction boundaries.

## The language

A lambda-calculus core with let-polymorphism:

```
let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib 30
```

Supports: `int`, `bool`, arithmetic, comparisons, boolean logic,
`if/else`, `let`, `let rec`, lambdas, currying, closures.

## Build & run

```bash
dune build
dune exec bin/main.exe            # run test suite
dune exec bin/main.exe -- --repl  # interactive REPL
```

Requires OCaml 5.x and dune 3.x (`brew install ocaml dune`).

## Steps completed

Each step is a git commit. Read the log to follow the progression.

| Step | What changed | Key idea |
|------|-------------|----------|
| 1 | Single-file naive evaluator | ADTs + pattern matching model a language naturally |
| 2 | Decompose into `Ast`, `Value`, `Env`, `Eval` modules with `.mli` interfaces | Abstract types hide representation; dune enforces the dependency graph |
| 3 | Hand-written `Lexer` + recursive-descent `Parser`; REPL | Precedence encoded in the call graph; mutable ref loops for left-assoc levels |
| 4 | Hindley-Milner type inference (Algorithm W) | Unification + substitution composition; `generalise`/`instantiate` for let-polymorphism |
| 5 | `Eval.Make(H : HOOKS)` functor; `Step_hooks`, `Trace_hooks` | One hook — `wrap : expr -> (unit -> value) -> value` — covers tracing, counting, and more |
| 6 | `Env` swapped from list to `Map.Make(String)`; `Memo_hooks` via `on_apply` | Abstract `Env.t` meant only two call sites changed; memoised `fib 30`: 26 M → 449 steps |

## Remaining steps

### Step 7 — Persistent REPL + top-level bindings

Extend the parser to accept `let x = e` (no `in`) as a top-level
statement. The REPL accumulates bindings across lines in a persistent
`Env.t` and `Typechecker.tenv`, so a session looks like OCaml's own
toplevel:

```
> let double = fun x -> x * 2
- : int -> int = <fun>
> double 21
- : int = 42
```

Key work: distinguish statement from expression in the parser; thread
the accumulated environment through the REPL loop.

### Step 8 — Proper error handling with `Result`

Replace `failwith`/`exception` with a `('a, error) result` type
throughout `Eval` and `Typechecker`. Define a structured `error` sum
type (`Unbound_var | Type_mismatch | Div_by_zero | …`) so callers can
pattern-match on failure causes rather than parsing exception strings.

Key work: propagate `Result.bind` (or a `let*` operator) through the
evaluator and type-checker without making the code unreadable.

### Step 9 — Tuples and product types

Add `(e1, e2)` syntax and `fst`/`snd` primitives (or full destructuring
`let (x, y) = e in …`). Extend `Typ.t` with `TProd of t * t`, the
parser with tuple literals, the type-checker with product unification,
and the evaluator with a `Tuple of Value.t * Value.t` constructor.

Key work: shows how all four pipeline stages (AST, parser, types, eval)
must be extended in lockstep for each new language feature.

### Step 10 — User-defined algebraic data types

Add `type option = None | Some of int` declarations (monomorphic first,
then polymorphic). Extend the type environment with a type-constructor
map and the value environment with constructor functions. Lay the
groundwork for pattern matching.

Key work: the type-checker gains a second environment (type definitions
alongside term types); constructors are first-class values.

### Step 11 — Pattern matching

Add `match e with | Pat -> e | …` syntax. Patterns cover literals,
variables, constructors, and tuples. The type-checker verifies
exhaustiveness (at least warn on non-exhaustive matches). The evaluator
does left-to-right pattern dispatch.

Key work: exhaustiveness checking is the hard part; a simple approach
is a coverage matrix over constructor sets.

### Step 12 — Bytecode compiler + stack VM

Replace the tree-walking evaluator with a two-phase pipeline:

1. `Compiler` — translate `Ast.expr` to a flat list of stack instructions
   (`PUSH_INT`, `ADD`, `CALL`, `RET`, `JUMP_IF_FALSE`, `CLOSURE`, …)
2. `Vm` — a simple stack machine that executes the instruction list

The `HOOKS` functor stays: `Vm.Make(H : VM_HOOKS)` intercepts
individual instructions instead of AST nodes.

Benefits over tree-walking: no recursion depth proportional to AST
depth; easier to add tail-call optimisation (detect `RET` after `CALL`,
reuse the stack frame); simpler serialisation of compiled programs.

Key work: closure compilation (capturing free variables at compile time
into a `MAKE_CLOSURE` instruction with an explicit free-variable list);
tail-call detection.
