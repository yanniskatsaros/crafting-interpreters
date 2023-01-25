module Lexer = Lox.Lexer
module Token = Lox.Token
module Error = Lox.Error

let run source =
  match Lexer.tokens source with
    | Ok tokens ->
      List.iter
        (fun token -> Format.printf "%s\n" (Token.show token))
        tokens
    | Error e -> Error.report e


let run_file filename =
  let source = In_channel.with_open_text filename In_channel.input_all in
  run source


let run_repl () =
  while true do
    try
      print_string ">> ";
      match In_channel.input_line In_channel.stdin with
        | Some line -> print_endline line; run line
        | None -> raise End_of_file
    with _ -> ()
  done


let () =
  match Array.length Sys.argv with
    | 1 -> run_repl ()
    | 2 -> run_file Sys.argv.(1)
    | _ ->
      print_endline "Usage: lox [script]";
      exit 64  (* book uses 64 in accordance with UNIX sysexits.h *)

