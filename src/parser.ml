include Token
include Lexer

module Parser = struct
  type parser = {
    lexer : Lexer.lexer;
    curr_token : Token.token;
    peek_token : Token.token;
  }

let next_token =
  let  parser.curr_token = parser.peek_token in
   parser.peek_token = parser.lexer.next_token in
   parser


  let new_parser = {lexer: Lexer.lexer; curToken: next_token; peekToken: next_token}

  let parse_program = ""

end
