(** Lexer: converts source strings into token streams. *)

type token =
  | Int    of int
  | Bool   of bool
  | Ident  of string
  (* arithmetic *)
  | Plus | Minus | Star | Slash
  (* comparison *)
  | Eq | Lt | Gt
  (* boolean *)
  | And | Or | Not
  (* keywords *)
  | If | Then | Else
  | Let | Rec | In
  | Fun | Arrow
  (* punctuation *)
  | Lparen | Rparen
  | Eof

val tokenize : string -> token list
(** [tokenize src] scans [src] and returns the token list.
    Raises [Failure] on unrecognised characters. *)
