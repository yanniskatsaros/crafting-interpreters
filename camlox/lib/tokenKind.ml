type t =
  (* single character tokens *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | SemiColon
  | Slash
  | Star
  (* one or two character tokens *)
  | Bang
  | BangEq
  | Eq
  | EqEq
  | Gt
  | Geq
  | Lt
  | Leq
  (* literals *)
  | Ident of string
  | StrLit of string
  | NumLit of float
  | Comment of string  (* not part of the book's implementation *)
  (* keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  [@@deriving show, eq]

