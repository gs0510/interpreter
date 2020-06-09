include Token
include Lexer

module Parser = struct
  type t = {
    lexer : Lexer.lexer;
    curr_token : Token.token;
    peek_token : Token.token;
  }

  let next_token t =
    let next_lexer, next_token = Lexer.next_token t.lexer in
    { lexer = next_lexer; curr_token = t.peek_token; peek_token = next_token }

  let new_parser t =
    let curr_lexer, cur_token = Lexer.next_token t.lexer in
    let peek_lexer, next_token = Lexer.next_token curr_lexer in
    { lexer = peek_lexer; curr_token = cur_token; peek_token = next_token }

  (*   let parse_let_statement t = match t.peek_token with
   *     | Ident ident ->
   *       Some (Ast.LetStatement )
   *     | _ -> None
   *)
end
