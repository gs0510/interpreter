open Interpreter
include Lexer
include Token

let basic_code = "5 + 1;"

let basic_code_expected_tokens =
  [
    { Token.token_type = Token.LET; literal = "let" };
    { Token.token_type = Token.IDENT; literal = "result" };
    { Token.token_type = Token.ASSIGN; literal = "=" };
    { Token.token_type = Token.INT; literal = "5" };
    { Token.token_type = Token.SEMICOLON; literal = ";" };
    { Token.token_type = Token.EOF; literal = "\x00" };
  ]

let monkey_source_code =
  "let five = 5;\n\
  \   let ten = 10;\n\
  \     let add = fn(x, y) {\n\
  \       x + y;\n\
  \   };\n\
  \   let result = add(five, ten);"

let source_code_expected_tokens =
  [
    { Token.token_type = Token.LET; literal = "let" };
    { Token.token_type = Token.IDENT; literal = "five" };
    { Token.token_type = Token.IDENT; literal = "five" };
    { Token.token_type = Token.ASSIGN; literal = "=" };
    { Token.token_type = Token.ASSIGN; literal = "=" };
    { Token.token_type = Token.INT; literal = "5" };
    { Token.token_type = Token.SEMICOLON; literal = ";" };
    { Token.token_type = Token.LET; literal = "let" };
    { Token.token_type = Token.IDENT; literal = "ten" };
    { Token.token_type = Token.ASSIGN; literal = "=" };
    { Token.token_type = Token.INT; literal = "10" };
    { Token.token_type = Token.SEMICOLON; literal = ";" };
    { Token.token_type = Token.LET; literal = "let" };
    { Token.token_type = Token.IDENT; literal = "add" };
    { Token.token_type = Token.ASSIGN; literal = "=" };
    { Token.token_type = Token.FUNCTION; literal = "fn" };
    { Token.token_type = Token.LPAREN; literal = "(" };
    { Token.token_type = Token.IDENT; literal = "x" };
    { Token.token_type = Token.COMMA; literal = "," };
    { Token.token_type = Token.IDENT; literal = "y" };
    { Token.token_type = Token.RPAREN; literal = ")" };
    { Token.token_type = Token.LBRACE; literal = "{" };
    { Token.token_type = Token.IDENT; literal = "x" };
    { Token.token_type = Token.PLUS; literal = "+" };
    { Token.token_type = Token.IDENT; literal = "y" };
    { Token.token_type = Token.SEMICOLON; literal = ";" };
    { Token.token_type = Token.RBRACE; literal = "}" };
    { Token.token_type = Token.SEMICOLON; literal = ";" };
    { Token.token_type = Token.LET; literal = "let" };
    { Token.token_type = Token.IDENT; literal = "result" };
    { Token.token_type = Token.ASSIGN; literal = "=" };
    { Token.token_type = Token.IDENT; literal = "add" };
    { Token.token_type = Token.LPAREN; literal = "(" };
    { Token.token_type = Token.IDENT; literal = "five" };
    { Token.token_type = Token.COMMA; literal = "," };
    { Token.token_type = Token.IDENT; literal = "ten" };
    { Token.token_type = Token.RPAREN; literal = ")" };
    { Token.token_type = Token.SEMICOLON; literal = ";" };
    { Token.token_type = Token.EOF; literal = "\x00" };
  ]

let token_testable = Alcotest.testable Token.pretty_print Token.tokens_eq

let test_next_token () =
  Alcotest.(check (list token_testable))
    "same list"
    [
      { Token.token_type = Token.ASSIGN; literal = "=" };
      { Token.token_type = Token.PLUS; literal = "+" };
      { Token.token_type = Token.LPAREN; literal = "(" };
      { Token.token_type = Token.RPAREN; literal = ")" };
      { Token.token_type = Token.LBRACE; literal = "{" };
      { Token.token_type = Token.RBRACE; literal = "}" };
      { Token.token_type = Token.COMMA; literal = "," };
      { Token.token_type = Token.SEMICOLON; literal = ";" };
      { Token.token_type = Token.EOF; literal = "\x00" };
    ]
    (Lexer.generate_tokens "=+(){},;")

let test_source_code () =
  Alcotest.(check (list token_testable))
    "test source code" source_code_expected_tokens
    (Lexer.generate_tokens monkey_source_code)

let test_basic_code () =
  Alcotest.(check (list token_testable))
    "test basic code" basic_code_expected_tokens
    (Lexer.generate_tokens basic_code)

let () =
  let open Alcotest in
  run "Utils"
    [
      ("check_next_token", [ test_case "Next token" `Quick test_next_token ]);
      ( "check_basic_source_code",
        [ test_case "Basic Source Code" `Quick test_basic_code ] );
      ("check-source-code", [ test_case "Source Code" `Quick test_source_code ]);
    ]
