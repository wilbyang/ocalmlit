(** Recursive-descent parser.

    Operator precedence (low → high):
      let / let rec / fun / if     (right-associative, no precedence climbing)
      ||
      &&
      =  <  >                      (non-associative)
      +  -                         (left-associative)
      *  /                         (left-associative)
      application                  (left-associative, tightest binary level)
      not e  (unary)
      atom: literal | variable | ( expr )
*)

val parse : string -> Ast.expr
(** [parse src] lexes and parses [src], returning an expression.
    Raises [Failure] on syntax errors. *)
