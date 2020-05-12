module Lexer = struct
  include Token

  type lexer = {
    input : string;
    position : int;
    read_position : int;
    ch : char;
  }

  let null_byte = '\x00'

  let read_char lexer =
    let new_ch =
      if lexer.read_position >= String.length lexer.input then null_byte
      else lexer.input.[lexer.read_position]
    in
    {
      lexer with
      position = lexer.read_position;
      read_position = lexer.read_position + 1;
      ch = new_ch;
    }

  let peek_char lexer =
    if lexer.read_position >= String.length lexer.input then null_byte
    else lexer.input.[lexer.read_position]

  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  (* Doesn't necessarily have to be a string, if it's let then the token is Token.LET etc etc. *)
  let read_identifier fn lexer =
    let position = lexer.position in
    let _ = Printf.printf "%d position\n" position in
    let rec read lex = if fn lex.ch then read_char lex |> read else lex in
    let updated_lex = read lexer in
    let _ =
      Printf.printf "%s\n"
        (String.sub updated_lex.input position
           (updated_lex.read_position - position - 1))
    in
    ( updated_lex,
      String.sub updated_lex.input position
        (updated_lex.read_position - position - 1) )

  let new_lexer input =
    let lexer = { input; position = 0; read_position = 0; ch = null_byte } in
    read_char lexer

  let rec next_token lexer =
    let skip l = l |> read_char |> next_token in
    let double_read l = l |> read_char |> read_char in
    match lexer.ch with
    | ' ' | '\n' | '\t' | '\r' -> skip lexer
    | '=' ->
        if peek_char lexer = '=' then
          (double_read lexer, { Token.token_type = Token.EQ; literal = "==" })
        else
          (read_char lexer, { Token.token_type = Token.ASSIGN; literal = "=" })
    | '(' ->
        (read_char lexer, { Token.token_type = Token.LPAREN; literal = "(" })
    | ';' ->
        (read_char lexer, { Token.token_type = Token.SEMICOLON; literal = ";" })
    | ')' ->
        (read_char lexer, { Token.token_type = Token.RPAREN; literal = ")" })
    | ',' -> (read_char lexer, { Token.token_type = Token.COMMA; literal = "," })
    | '+' -> (read_char lexer, { Token.token_type = Token.PLUS; literal = "+" })
    | '{' ->
        (read_char lexer, { Token.token_type = Token.LBRACE; literal = "{" })
    | '}' ->
        (read_char lexer, { Token.token_type = Token.RBRACE; literal = "}" })
    | '!' ->
        if peek_char lexer = '=' then
          ( double_read lexer,
            { Token.token_type = Token.NOT_EQ; literal = "!=" } )
        else (read_char lexer, { Token.token_type = Token.BANG; literal = "!" })
    | '*' ->
        (read_char lexer, { Token.token_type = Token.ASTERISK; literal = "*" })
    | '-' -> (read_char lexer, { Token.token_type = Token.MINUS; literal = "-" })
    | '>' -> (read_char lexer, { Token.token_type = Token.GT; literal = ">" })
    | '<' -> (read_char lexer, { Token.token_type = Token.LT; literal = "<" })
    | '/' -> (read_char lexer, { Token.token_type = Token.SLASH; literal = "/" })
    | '\x00' -> (lexer, { Token.token_type = Token.EOF; literal = "\x00" })
    | c ->
        if is_alpha c then
          let new_lexer, ident = read_identifier is_alpha lexer in
          let token_string_type = Token.lookup ident in
          (new_lexer, { Token.token_type = token_string_type; literal = ident })
        else if is_digit c then
          let new_lexer, ident = read_identifier is_digit lexer in
          (new_lexer, { Token.token_type = Token.INT; literal = ident })
        else (lexer, { Token.token_type = Token.ILLEGAL; literal = "" })

  let generate_tokens input_string =
    let lexer = new_lexer input_string in
    let rec gen lxr tokens =
      match next_token lxr with
      | _, ({ Token.token_type = Token.EOF; literal = "\x00" } as t) ->
          List.rev_append tokens [ t ]
      | l, tok -> gen l (tok :: tokens)
    in
    gen lexer []
end
