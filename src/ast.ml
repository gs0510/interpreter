include Token

module type Node = sig
  type t

  val token_literal : t -> string
end

include Token

module Statement : Node = struct
  type t = Token.token

  let token_literal t = Token.token_to_string t
end

let program : (module Node) list = [ (module Statement) ]
