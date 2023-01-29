(* open Token *)

type 'a t = string -> ('a * string) option


(** Returns [char option] at index [i] of the given string [str] *)
let ( .?[] ) str i =
  try Some ( str.[i] ) with _ -> None


(** Returns [true] if the given character [c] is ASCII alphabetic *)
let is_alphabetic = function
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false


(** Returns [true] if the given character [c] is ASCII digit *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false


(** Returns [true] if the given character [c] is ASCII alphanumeric *)
let is_alphanumeric = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9' -> true
  | _ -> false


(** Returns [true] if the given character [c] is ASCII space or tab [\t] *)
let is_space = function
  | ' ' | '\t' -> true
  | _ -> false


(** Returns [true] if the given character [c] is ASCII newline [\n] *)
let is_newline = function
  | '\n' -> true
  | _ -> false


(** Returns [true] if the given character [c] is ASCII space, tab, or newline *)
let is_whitespace c =
  (is_space c) || (is_newline c)


(** Recognizes a pattern, and returns the part of the input that matches it *)
let tag pattern input =
  let n_pat = String.length pattern in
  let n_inp = String.length input in
  match String.starts_with input ~prefix:pattern with
    | true ->
      let rem = String.sub input n_pat (n_inp - n_pat) in
      Some (pattern, rem)
    | false -> None


(** Consumes the next character in the string slice provided there is at least one *)
let next input =
  try
    let n_inp = String.length input in
    let c = input.[0] in
    let rem = String.sub input 1 (n_inp - 1) in
    Some (c, rem)
  with (Invalid_argument _) -> None


(** Steps the lexer if the next character matches the given character *)
let match_char c input =
  match input.?[0] with
    | Some c' when c = c' -> next input
    | _ -> None


(** Returns the first [n] characters as a string slice from the input *)
let take n input =
  try
    let n_inp = String.length input in
    let ret = String.sub input 0 n in
    let rem = String.sub input n (n_inp - n) in
    Some (ret, rem)
  with (Invalid_argument _) -> None


(** Returns the longest input slice (if any) that matches the predicate *)
let take_while pred input =
  let n_inp = String.length input in

  let rec go str idx =
    match str.?[idx] with
      | None ->
        let ret = String.sub str 0 idx in
        let rem = String.sub str idx (n_inp - idx) in
        Some (ret, rem)
      | Some c ->
        if (pred c)
          then go str (idx + 1)
        else
          let ret = String.sub str 0 idx in
          let rem = String.sub str idx (n_inp - idx) in
          Some (ret, rem)
  in go input 0


(** Returns the longest input slice (for at least 1 character) that matches the predicate *)
let take_while1 pred input =
  match input.?[0] with
    | Some c when pred c -> take_while pred input
    | _ -> None


(** Recognizes zero or more spaces or tabs *)
let space0 input = 
  take_while is_space input


(** Recognizes zero or more spaces, tabs, or newline characters *)
let whitespace0 input =
  take_while is_whitespace input


(** Matches an object from the first parser and discards it, then gets the objects
    from the second parser, and finally matches an object from the third parser
    and discards it *)
let delimited opening inner closing input =
  match opening input with
    | None -> None
    | Some (_, inp) -> begin
      match inner inp with
        | None -> None
        | Some (ret, inp') -> begin
          match closing inp' with
            | None -> None
            | Some (_, rest) -> Some (ret, rest)
        end
    end


(** Matches on a sequence of parsers only returning the final output if all parsers matched *)
let sequence parsers input =
  let rec go parsers input acc =
    match parsers with
      | [] -> None
      | p :: [] -> begin
        match p input with
          | None -> None
          | Some (ret, rem) ->
              let chars = List.rev (ret :: acc) in
              let ret = String.concat "" chars in
              Some (ret, rem)
      end
      | p :: parsers' -> begin
        match p input with
          | None -> None
          | Some (ret, input') -> go parsers' input' (ret :: acc)
      end
    in
    go parsers input []


(** Matches on an "identifier" which is defined as a first alphabetic or underscore character
    followed by zero or more alphanumeric characters or underscores *)
let ident input =

  let is_valid_first c =
    (is_alphabetic c) || (c = '_')
  in

  let is_valid_char c =
    (is_alphanumeric c) || (c = '_')
  in

  match take 1 input with
    | None -> None
    | Some (c, _) -> begin
      let c = c.[0] in (* parser a single char string to char *)
      (* an identifier must start with a letter or an underscore *)
      if is_valid_first c then 
        take_while is_valid_char input
      else
        None
    end
  (* delimited space0 ident' space0 input *)


(** Attempts each parser in the list one-by-one in succession, returning on the first success *)
let rec alt parsers input =
  match parsers with
    | [] -> None
    | p :: parsers' -> begin
      match p input with
        | None -> alt parsers' input
        | Some (ret, rem) -> Some (ret, rem)
    end


(** Matches a number literal *)
let number = alt [
    (sequence [(take_while1 is_digit); (tag "."); (take_while is_digit)]) ;
    (sequence [(tag "."); (take_while1 is_digit)]) ;
    (take_while1 is_digit)
  ]


(** Matches a string literal delimited by double-quotes (and performs escaping) *)
let string input =
  let rec consume ~escaped acc i inp = match escaped with
    (* do not escape the next character *)
    | false -> begin match inp.?[i] with
      (* backslash is the escape character, escape the next character encountered *)
      | Some '\\' -> consume ~escaped:true acc (i+1) inp
      (* reached the end of the string to consume *)
      | Some '"' -> begin
        let ret = acc |> List.rev |> List.to_seq |> String.of_seq in
        let n_inp = String.length inp in
        let i = i + 1 in (* accounts for the final double quote matched *)
        let rem = String.sub inp i (n_inp - i) in
        Some (ret, rem)
      end
      | Some c -> consume ~escaped:false (c :: acc) (i+1) inp
      | None -> None
    end
    (* escape the next character *)
    | true -> begin match inp.?[i] with
      | Some 't' -> consume ~escaped:false ('\t' :: acc) (i+1) inp
      | Some 'n' -> consume ~escaped:false ('\n' :: acc) (i+1) inp
      | Some 'r' -> consume ~escaped:false ('\r' :: acc) (i+1) inp
      | Some '"' -> consume ~escaped:false ('"'  :: acc) (i+1) inp
      (* handle an unknown escaped character simply as a regular character (good/bad idea?) *)
      | Some c   -> consume ~escaped:false (c    :: acc) (i+1) inp
      | None -> None
    end
  in
  match input.?[0] with
    | Some '"' -> consume ~escaped:false [] 1 input
    | _ -> None


(* === Lox specific parsers returning tokens === *)


let one_char_token input =
  match take 1 input with
    | None -> None
    | Some (c, rem) -> begin
      match c with
        | "(" -> Some (TokenKind.LParen, rem)
        | ")" -> Some (TokenKind.RParen, rem)
        | "{" -> Some (TokenKind.LBrace, rem)
        | "}" -> Some (TokenKind.RBrace, rem)
        | "," -> Some (TokenKind.Comma, rem)
        | "." -> Some (TokenKind.Dot, rem)
        | "-" -> Some (TokenKind.Minus, rem)
        | "+" -> Some (TokenKind.Plus, rem)
        | ";" -> Some (TokenKind.SemiColon, rem)
        | "*" -> Some (TokenKind.Star, rem)
        | "/" -> Some (TokenKind.Slash, rem)
        | "!" -> Some (TokenKind.Bang, rem)
        | "=" -> Some (TokenKind.Eq, rem)
        | ">" -> Some (TokenKind.Gt, rem)
        | "<" -> Some (TokenKind.Lt, rem)
        | _ -> None
      end


let two_char_token input =
  match take 2 input with
    | None -> None
    | Some (ret, rem) -> begin
      match ret with
        | "!=" -> Some (TokenKind.BangEq, rem)
        | "==" -> Some (TokenKind.EqEq, rem)
        | ">=" -> Some (TokenKind.Geq, rem)
        | "<=" -> Some (TokenKind.Leq, rem)
        | "//" -> begin
          match take_while (fun c -> c <> '\n') rem with
            | Some (com, rem') -> Some (TokenKind.Comment(com), rem')
            | None -> None
        end
        | _ -> None
      end


let keyword_or_ident input =
  match ident input with
    | None -> None
    | Some (ret, rem) -> begin
      match ret with
        | "and" -> Some (TokenKind.And, rem)
        | "class" -> Some (TokenKind.Class, rem)
        | "else" -> Some (TokenKind.Else, rem)
        | "false" -> Some (TokenKind.False, rem)
        | "fun" -> Some (TokenKind.Fun, rem)
        | "for" -> Some (TokenKind.For, rem)
        | "if" -> Some (TokenKind.If, rem)
        | "nil" -> Some (TokenKind.Nil, rem)
        | "or" -> Some (TokenKind.Or, rem)
        | "print" -> Some (TokenKind.Print, rem)
        | "return" -> Some (TokenKind.Return, rem)
        | "super" -> Some (TokenKind.Super, rem)
        | "this" -> Some (TokenKind.This, rem)
        | "true" -> Some (TokenKind.True, rem)
        | "var" -> Some (TokenKind.Var, rem)
        | "while" -> Some (TokenKind.While, rem)
        | i -> Some (TokenKind.Ident(i), rem)
      end


let string_literal input =
  match string input with
    | None -> None
    | Some (s, rem) -> Some (TokenKind.StrLit(s), rem)


let number_literal input =
  match number input with
    | None -> None
    | Some (n, rem) -> begin
      let n = Float.of_string n in
      Some (TokenKind.NumLit(n), rem)
    end


(* FIXME: remove after including in two_char_token *)
let comment input =
  let comment' = sequence [
    (tag "//");
    (take_while (fun c -> c <> '\n'))
  ]
  in
  match comment' input with
    | Some (ret, rem) -> Some (TokenKind.Comment(ret), rem)
    | None -> None


let token =
  let token' = alt [
    two_char_token;
    one_char_token;
    keyword_or_ident;
    string_literal;
    number_literal;
  ] in
  delimited whitespace0 token' whitespace0
    


let lex input =
  let rec go acc input =
    match token input with
      | Some (token, rem) -> go (token :: acc) rem
      | None -> List.rev acc
  in
  Ok (go [] input)

