module Lexer = Lox.Lexer
module TokenKind = Lox.TokenKind
module Error = Lox.Error

let repl_banner =
  let lox_ascii_art =
    {|
     __         ______     __  __
    /\ \       /\  __ \   /\_\_\_\
    \ \ \____  \ \ \/\ \  \/_/\_\/_
     \ \_____\  \ \_____\   /\_\/\_\
      \/_____/   \/_____/   \/_/\/_/
    |}
  in
  (Format.sprintf
    "%s\n=== Welcome to the Lox REPL! Type <Ctrl+D> to quit. ===\n"
    lox_ascii_art)


let run source =
  match Lexer.lex source with
    | Ok token_kinds ->
      List.iter
        (fun tk -> Format.printf "%s\n" (TokenKind.show tk))
        token_kinds
    | Error e -> Error.report e


let run_file filename =
  let source = In_channel.with_open_text filename In_channel.input_all in
  run source


let run_repl () =
  let write str =
    Out_channel.output_string Out_channel.stdout str;
    Out_channel.flush Out_channel.stdout
  in
  let input () = In_channel.input_line In_channel.stdin in

  write repl_banner;
  try
    let i = ref 1 in
    while true do
      write (Format.sprintf "\n[%d]: " !i);
      match input () with
        | Some line ->
            run line;
            i := !i + 1
        | None -> raise End_of_file
    done
  with _ -> ()


let () =
  match Array.length Sys.argv with
    | 1 -> run_repl ()
    | 2 -> run_file Sys.argv.(1)
    | _ ->
      print_endline "Usage: lox [script]";
      exit 64  (* book uses 64 in accordance with UNIX sysexits.h *)

