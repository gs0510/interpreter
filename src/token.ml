module Token = struct
  type token_type =
    | ILLEGAL
    | EOF
    | IDENT
    | INT
    | ASSIGN
    | PLUS
    | COMMA
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | FUNCTION
    | LET

  let token_to_string = function
    | ILLEGAL -> "ILLEGAL"
    | EOF -> "EOF"
    | IDENT -> "IDENT"
    | INT -> "INT"
    | ASSIGN -> "ASSIGN"
    | PLUS -> "PLUS"
    | COMMA -> "COMMA"
    | SEMICOLON -> "SEMICOLON"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | FUNCTION -> "FUNCTION"
    | LET -> "LET"

  let tokens_eq tok_a tok_b = tok_a = tok_b

  let pretty_print ppf tok = Fmt.pf ppf "%s" (token_to_string tok)
end