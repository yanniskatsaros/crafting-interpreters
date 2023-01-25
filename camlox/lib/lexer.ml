open TokenKind
open Token

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


(** Recognizes a pattern, and returns the part of the input that matches it *)
let tag pattern input =
  let n_pat = String.length pattern in
  let n_inp = String.length input in
  match String.starts_with input ~prefix:pattern with
    | true ->
      let rem = String.sub input n_pat (n_inp - n_pat) in
      Some (pattern, rem)
    | false -> None


(** Returns the longest input slice that matches the predicate *)
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



