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
      position = lexer.position;
      read_position = lexer.read_position + 1;
      ch = new_ch;
    }

  let new_lexer input =
    let lexer = { input; position = 0; read_position = 0; ch = null_byte } in
    read_char lexer

  let next_token lexer =
    match lexer.ch with
    | '=' -> (read_char lexer, Token.ASSIGN)
    | ';' -> (read_char lexer, Token.SEMICOLON)
    | '(' -> (read_char lexer, Token.LPAREN)
    | ')' -> (read_char lexer, Token.RPAREN)
    | ',' -> (read_char lexer, Token.COMMA)
    | '+' -> (read_char lexer, Token.PLUS)
    | '{' -> (read_char lexer, Token.LBRACE)
    | '}' -> (read_char lexer, Token.RBRACE)
    | '\x00' -> (lexer, Token.EOF)
    | _ -> failwith "unmatched character"

  let generate_tokens input_string =
    let lexer = new_lexer input_string in
    let rec gen lxr tokens =
      match next_token lxr with
      | _, Token.EOF -> List.rev_append tokens [ Token.EOF ]
      | l, tok -> gen l (tok :: tokens)
    in
    gen lexer []
end
