module Token = struct
  type token_type =
    | ILLEGAL
    | EOF
    | IDENT
    | INT
    | ASSIGN
    | PLUS
    | MINUS
    | BANG
    | ASTERISK
    | SLASH
    | LT
    | GT
    | COMMA
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | FUNCTION
    | LET
    | TRUE
    | FALSE
    | IF
    | ELSE
    | RETURN
    | EQ
    | NOT_EQ

  type token = { token_type : token_type; literal : string }

  let token_to_string token =
    match token.token_type with
    | ILLEGAL -> "ILLEGAL"
    | EOF -> "EOF"
    | IDENT -> "IDENT"
    | INT -> "INT"
    | ASSIGN -> "ASSIGN"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | BANG -> "BANG"
    | ASTERISK -> "ASTERISK"
    | SLASH -> "SLASH"
    | LT -> "LT"
    | GT -> "GT"
    | COMMA -> "COMMA"
    | SEMICOLON -> "SEMICOLON"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | FUNCTION -> "FUNCTION"
    | LET -> "LET"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | RETURN -> "RETURN"
    | EQ -> "EQUAL"
    | NOT_EQ -> "NOT_EQ"

  let lookup = function
    | "let" -> LET
    | "fn" -> FUNCTION
    | "if" -> IF
    | "else" -> ELSE
    | "return" -> RETURN
    | "true" -> TRUE
    | "false" -> FALSE
    | _ -> IDENT

  let tokens_eq tok_a tok_b = tok_a.token_type = tok_b.token_type

  let pretty_print ppf tok = Fmt.pf ppf "%s" (token_to_string tok)
end
