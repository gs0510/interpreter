include Token

module type Node = sig
  type t

  val token_literal : t -> string
end

module Statement : sig
  include Node

  val statement_node : t -> t
end = struct
  type t = Token.token

  let token_literal t = Token.token_to_string t

  let statement_node t = t
end

module Expression : sig
  include Node

  val expression_node : t -> t
end = struct
  type t = Token.token

  let token_literal t = Token.token_to_string t

  let expression_node t = t
end

module Identifier = struct
  type t = { token : Token.token; value : string }
end

module LetStatement = struct
  type t = { token : Token.token; name : Identifier.t; value : Expression.t }
end

module Node' = struct
  type t = E of Expression.t | S of Statement.t
end

type program = Node'.t list

(* type node = {token: Token.token} *)

(* type identifier = {token: Token.token; value: string;} *)

(* type expression = {node: node; token: Token.token; } *)

(* type statement = {} *)
