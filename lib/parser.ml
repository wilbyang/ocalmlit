open Ast
open Lexer

(* ---- Token stream ---- *)

type stream = { mutable tokens : token list }

let make_stream tokens = { tokens }

let peek s =
  match s.tokens with
  | []     -> Eof
  | t :: _ -> t

let consume s =
  match s.tokens with
  | []     -> Eof
  | t :: rest -> s.tokens <- rest; t

let expect s tok =
  let got = consume s in
  if got <> tok then
    failwith (Printf.sprintf "Parser: expected %s"
      (match tok with
       | In    -> "'in'"   | Then  -> "'then'" | Else  -> "'else'"
       | Arrow -> "'->'"   | Eq    -> "'='"    | Lparen -> "'('"
       | Rparen -> "')'"   | _     -> "token"))

(* ---- Grammar (precedence low → high) ----

   expr   ::= let_expr | fun_expr | if_expr | or_expr
   let_expr ::= 'let' 'rec' ident ident '=' expr 'in' expr
              | 'let' ident '=' expr 'in' expr
   fun_expr ::= 'fun' ident '->' expr
   if_expr  ::= 'if' expr 'then' expr 'else' expr
   or_expr  ::= and_expr ('||' and_expr)*
   and_expr ::= cmp_expr ('&&' cmp_expr)*
   cmp_expr ::= add_expr (('=' | '<' | '>') add_expr)?
   add_expr ::= mul_expr (('+' | '-') mul_expr)*
   mul_expr ::= app_expr (('*' | '/') app_expr)*
   app_expr ::= unary_expr+          (left-associative juxtaposition)
   unary_expr ::= 'not' unary_expr | atom
   atom     ::= INT | BOOL | ident | '(' expr ')'
*)

let rec parse_expr s =
  match peek s with
  | Let -> parse_let s
  | Fun -> parse_fun s
  | If  -> parse_if  s
  | _   -> parse_or  s

and parse_let s =
  expect s Let;
  match peek s with
  | Rec ->
    ignore (consume s);
    let fname = expect_ident s in
    let param = expect_ident s in
    expect s Eq;
    let def  = parse_expr s in
    expect s In;
    let body = parse_expr s in
    Let_rec (fname, param, def, body)
  | _ ->
    let name = expect_ident s in
    expect s Eq;
    let def  = parse_expr s in
    expect s In;
    let body = parse_expr s in
    Let (name, def, body)

and parse_fun s =
  expect s Fun;
  let param = expect_ident s in
  expect s Arrow;
  let body = parse_expr s in
  Fun (param, body)

and parse_if s =
  expect s If;
  let cond  = parse_expr s in
  expect s Then;
  let then_ = parse_expr s in
  expect s Else;
  let else_ = parse_expr s in
  If (cond, then_, else_)

and parse_or s =
  let left = ref (parse_and s) in
  while peek s = Or do
    ignore (consume s);
    left := Binop ("||", !left, parse_and s)
  done;
  !left

and parse_and s =
  let left = ref (parse_cmp s) in
  while peek s = And do
    ignore (consume s);
    left := Binop ("&&", !left, parse_cmp s)
  done;
  !left

and parse_cmp s =
  let left = parse_add s in
  match peek s with
  | Eq -> ignore (consume s); Binop ("=",  left, parse_add s)
  | Lt -> ignore (consume s); Binop ("<",  left, parse_add s)
  | Gt -> ignore (consume s); Binop (">",  left, parse_add s)
  | _  -> left

and parse_add s =
  let left = ref (parse_mul s) in
  let continue_ = ref true in
  while !continue_ do
    match peek s with
    | Plus  -> ignore (consume s); left := Binop ("+", !left, parse_mul s)
    | Minus -> ignore (consume s); left := Binop ("-", !left, parse_mul s)
    | _     -> continue_ := false
  done;
  !left

and parse_mul s =
  let left = ref (parse_app s) in
  let continue_ = ref true in
  while !continue_ do
    match peek s with
    | Star  -> ignore (consume s); left := Binop ("*", !left, parse_app s)
    | Slash -> ignore (consume s); left := Binop ("/", !left, parse_app s)
    | _     -> continue_ := false
  done;
  !left

(* application: left-associative juxtaposition of atoms/unaries *)
and parse_app s =
  let func = ref (parse_unary s) in
  let continue_ = ref true in
  while !continue_ do
    match peek s with
    (* tokens that can start an argument *)
    | Int _ | Bool _ | Ident _ | Lparen | Not ->
      func := App (!func, parse_unary s)
    | _ -> continue_ := false
  done;
  !func

and parse_unary s =
  match peek s with
  | Not -> ignore (consume s); Unop ("not", parse_unary s)
  | _   -> parse_atom s

and parse_atom s =
  match consume s with
  | Int  n    -> Lit_int n
  | Bool b    -> Lit_bool b
  | Ident id  -> Var id
  | Lparen    ->
    let e = parse_expr s in
    expect s Rparen;
    e
  | tok -> failwith (Printf.sprintf "Parser: unexpected token in atom: %s"
      (match tok with
       | Eof   -> "EOF"    | Plus  -> "+"    | Minus -> "-"
       | Star  -> "*"      | Slash -> "/"    | Eq    -> "="
       | Lt    -> "<"      | Gt    -> ">"    | And   -> "&&"
       | Or    -> "||"     | If    -> "if"   | Then  -> "then"
       | Else  -> "else"   | Let   -> "let"  | Rec   -> "rec"
       | In    -> "in"     | Fun   -> "fun"  | Arrow -> "->"
       | Rparen -> ")"     | _     -> "?"))

and expect_ident s =
  match consume s with
  | Ident id -> id
  | _ -> failwith "Parser: expected identifier"

(* ---- Public entry point ---- *)

let parse src =
  let tokens = Lexer.tokenize src in
  let stream  = make_stream tokens in
  let expr    = parse_expr stream in
  (match peek stream with
   | Eof -> ()
   | _   -> failwith "Parser: unexpected tokens after expression");
  expr
