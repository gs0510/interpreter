open Interpreter
include Lexer
include Token

let token_testable = Alcotest.testable Token.pretty_print Token.tokens_eq

let test_next_token () =
  Alcotest.(check (list token_testable))
    "same list"
    [
      Token.ASSIGN;
      Token.PLUS;
      Token.LPAREN;
      Token.RPAREN;
      Token.LBRACE;
      Token.RBRACE;
      Token.COMMA;
      Token.SEMICOLON;
      Token.EOF;
    ]
    (Lexer.generate_tokens "=+(){},;")

let () =
  let open Alcotest in
  run "Utils"
    [
      ("check-basic-tokens", [ test_case "Basic tokens" `Quick test_next_token ]);
    ]
