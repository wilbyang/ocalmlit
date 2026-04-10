type token =
  | Int    of int
  | Bool   of bool
  | Ident  of string
  | Plus | Minus | Star | Slash
  | Eq | Lt | Gt
  | And | Or | Not
  | If | Then | Else
  | Let | Rec | In
  | Fun | Arrow
  | Lparen | Rparen
  | Eof

let keyword_map = [
  "true",  Bool true;
  "false", Bool false;
  "not",   Not;
  "if",    If;
  "then",  Then;
  "else",  Else;
  "let",   Let;
  "rec",   Rec;
  "in",    In;
  "fun",   Fun;
]

let tokenize src =
  let len = String.length src in
  let pos = ref 0 in
  let tokens = Buffer.create 16 in   (* we build a list via recursion *)
  ignore tokens;

  let peek () = if !pos < len then Some src.[!pos] else None in
  let advance () = incr pos in
  let eat_while pred =
    let buf = Buffer.create 8 in
    let rec loop () =
      match peek () with
      | Some c when pred c -> Buffer.add_char buf c; advance (); loop ()
      | _ -> ()
    in
    loop (); Buffer.contents buf
  in

  let rec next_token () =
    match peek () with
    | None -> Eof
    | Some c when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
      advance (); next_token ()
    | Some '(' -> advance (); Lparen
    | Some ')' -> advance (); Rparen
    | Some '+' -> advance (); Plus
    | Some '-' ->
      advance ();
      if peek () = Some '>' then (advance (); Arrow) else Minus
    | Some '*' -> advance (); Star
    | Some '/' ->
      advance ();
      (* line comment: // ... *)
      if peek () = Some '/' then begin
        while peek () <> Some '\n' && peek () <> None do advance () done;
        next_token ()
      end else Slash
    | Some '=' -> advance (); Eq
    | Some '<' -> advance (); Lt
    | Some '>' -> advance (); Gt
    | Some '&' ->
      advance ();
      if peek () = Some '&' then (advance (); And)
      else failwith "Lexer: expected '&&'"
    | Some '|' ->
      advance ();
      if peek () = Some '|' then (advance (); Or)
      else failwith "Lexer: expected '||'"
    | Some c when c >= '0' && c <= '9' ->
      let s = eat_while (fun c -> c >= '0' && c <= '9') in
      Int (int_of_string s)
    | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
      let s = eat_while (fun c ->
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') || c = '_' || c = '\'') in
      (match List.assoc_opt s keyword_map with
       | Some tok -> tok
       | None     -> Ident s)
    | Some c ->
      failwith (Printf.sprintf "Lexer: unexpected character '%c'" c)
  in

  let rec collect acc =
    match next_token () with
    | Eof -> List.rev (Eof :: acc)
    | tok -> collect (tok :: acc)
  in
  collect []
