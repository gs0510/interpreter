open Interpreter
include Lexer
include Token

let prompt = "<<"

let rec start () =
  let input = read_line () in
  if input = "" then ()
  else
    let tokens = Lexer.generate_tokens input in
    List.iter (fun token -> print_endline (Token.token_to_string token)) tokens;
    start ()
